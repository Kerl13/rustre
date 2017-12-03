open Ast_normalized

module type Scheduling = sig
  exception SchedulingError of string
  val schedule : Ast_normalized.nfile -> string -> Ast_normalized.nfile
end

module Simple = struct
  (** Public exception *)
  exception SchedulingError of string

  (** Private exceptions *)
  exception Not_a_node of string
  exception MultipleDefinitions of string * string
  exception CausalityError of string * nequation list

  (** Equations with a unique identifier *)
  module EqWithId = struct
    type t = int * nequation
    let compare (e1 : t) (e2 : t) = Pervasives.compare (fst e1) (fst e2)
    let different (e1 : t) (e2 : t) = compare e1 e2 <> 0
  end
  module Smap = Map.Make(String)
  module EqMap = Map.Make(EqWithId)

  (** Dependency graph *)
  module Graph = struct
    type t = EqWithId.t list EqMap.t

    let empty = EqMap.empty

    exception Cycle of t

    let keys (g : t) = EqMap.fold (fun (_, eq) _ keys -> eq :: keys) g []

    let add_vertex (g : t) eq =
      if EqMap.mem eq g then g
      else EqMap.add eq [] g

    let add_edge (g : t) eq1 eq2 =
      (* If [eq2] has no binding in [g], bind it to the empty list *)
      let g = add_vertex g eq2 in
      if EqMap.mem eq1 g then
        EqMap.add eq1 (eq2 :: (EqMap.find eq1 g)) g
      else
        EqMap.add eq1 [eq2] g

    let pop_leaf (g : t) =
      let key =
        let leaves = EqMap.filter (fun _ -> Misc.is_empty_list) g in
        if EqMap.is_empty leaves then raise (Cycle g);
        EqMap.choose leaves |> fst
      in
      let g = g |>
              EqMap.remove key |>
              EqMap.map (List.filter (EqWithId.different key))
      in g, key

    let rec topo_sort eqs (g : t) =
      if EqMap.is_empty g then List.rev eqs
      else
        let (g, (_, eq)) = pop_leaf g in
        topo_sort (eq :: eqs) g
    let topo_sort = topo_sort []
  end

  (** Find free variables in exprssions *)
  let rec vars_expr : type a. string list -> a nexpr -> string list
    = fun vars e -> match e.nexpr_desc with
    | NConst _ -> vars
    | NIdent x -> (x :: vars)
    | NBOp (_, e1, e2) -> vars_expr (vars_expr vars e1) e2
    | NUOp (_, e) -> vars_expr vars e
    | NWhen (e, _, x) -> vars_expr (x :: vars) e

  (** Find free variables in merge expressions *)
  and vars_emerge vars e = match e.nexpr_merge_desc with
    | NMerge (x, cases) ->
      List.fold_left (fun vars (_, e) -> vars_emerge vars e) (x :: vars) cases
    | NExpr e -> vars_expr vars e

  let number_and_resolve_deps node_name (i, eqs, defmap) eq =
    let add x defmap =
      if Smap.mem x defmap then raise (MultipleDefinitions (node_name, x));
      Smap.add x (i, eq) defmap
    in
    let defmap, vars = match eq with
      | EquSimple (x, e) -> add x defmap, vars_emerge [] e
      | EquFby (x, _, _) -> add x defmap, []
      | EquApp (pat, _, arg, every) ->
        let varlist = pat.Ast_typed.pat_desc in
        let defmap = Ast_typed_utils.var_list_fold (fun defmap x -> add x defmap) defmap varlist in
        let vars = Ast_typed_utils.var_list_fold (fun vars v -> v :: vars) [] arg in
        let vars = vars_expr vars every in
        defmap, vars
    in (i+1, (i, eq, vars) :: eqs, defmap)

  let number_and_build_defmap node_name eqs =
    let (_, eqs, defmap) = List.fold_left (number_and_resolve_deps node_name) (0, [], Smap.empty) eqs in
    eqs, defmap

  let schedule_node scheduled (NNode node) =
    let (node_name, (eqs, defmap)) =
      let Ast_typed.Tagged (_, _, node_name) = node.n_name in
      node_name, number_and_build_defmap node_name node.n_eqs
    in
    let add_edge_opt ieq g x =
      try
        let ieq2 = Smap.find x defmap in
        Graph.add_edge g ieq ieq2
      with
      | Not_found -> g
    in
    let g = List.fold_left
        (fun g (i, eq, vars) ->
           let g = Graph.add_vertex g (i, eq) in
           List.fold_left (add_edge_opt (i, eq)) g vars)
      Graph.empty
      eqs
    in
    let eqs =
      try Graph.topo_sort g
      with Graph.Cycle g -> raise (CausalityError (node_name, Graph.keys g)) in
    NNode { node with n_eqs = eqs } :: scheduled

  let rec cut_at acc name = function
    | [] -> raise (Not_a_node name)
    | NNode n :: nodes ->
      let Ast_typed.Tagged (_, _, name') = n.n_name in
      if name' = name
      then NNode n :: acc
      else cut_at (NNode n :: acc) name nodes
  let cut_at name = cut_at [] name

  let schedule file main =
    let nodes = cut_at main file.nf_nodes in
    let scheduled = List.fold_left schedule_node [] nodes in
    { file with nf_nodes = scheduled }

  let schedule file main =
    try schedule file main
    with
    | Not_a_node name ->
      let message = Format.sprintf "Unbound node %s" name in
      raise (SchedulingError message)
    | MultipleDefinitions (node_name, x) ->
      let message = Format.sprintf "Variable %s is defined multiple times in node %s" x node_name in
      raise (SchedulingError message)
    | CausalityError (node_name, eqs) ->
      let message = Format.asprintf
        "Cyclic dependencies in node %s between the following equations:\n  %a"
        node_name (Misc.pp_list "\n  " pp_eq) eqs
      in
      raise (SchedulingError message)
end

module Stupid = struct
  exception SchedulingError of string
  let schedule file _name = file
end
