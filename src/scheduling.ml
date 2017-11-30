open Ast_normalized

module type Scheduling = sig
  val schedule : Ast_normalized.nfile -> string -> Ast_normalized.nfile
end

module Simple = struct
  exception Not_a_node of string
  exception MutlipleDefinitions of string * string

  module EqWithId = struct
    type t = int * nequation
    let compare (e1 : t) (e2 : t) = Pervasives.compare (fst e1) (fst e2)
  end
  module Smap = Map.Make(String)
  module EqMap = Map.Make(EqWithId)

  module Graph = struct
    type t = EqWithId.t list EqMap.t

    exception Cycle

    let empty = EqMap.empty

    let add_edge (g : t) eq1 eq2 =
      let g =
        if (EqMap.mem eq2 g) then g
        else EqMap.add eq2 [] g
      in
      if EqMap.mem eq1 g then
        EqMap.add eq1 (eq2 :: (EqMap.find eq1 g)) g
      else
        EqMap.add eq1 [eq2] g

    let empty_list = function
      | [] -> true
      | _ -> false

    let pop_leaf (g : t) =
      let key =
        let leaves = EqMap.filter (fun _ -> empty_list) g in
        if EqMap.is_empty leaves then raise Cycle;
        EqMap.choose leaves |> fst
      in
      let g = g |>
              EqMap.remove key |>
              EqMap.map (List.filter ((!=) key))
      in g, key

    let rec topo_sort eqs (g : t) =
      if EqMap.is_empty g then List.rev eqs
      else
        let (g, (_, eq)) = pop_leaf g in
        topo_sort (eq :: eqs) g
    let topo_sort = topo_sort []
  end

  let rec vars_expr : type a. string list -> a nexpr -> string list
    = fun vars e -> match e.nexpr_desc with
    | NConst _ -> vars
    | NIdent x -> (x :: vars)
    | NBOp (_, e1, e2) -> vars_expr (vars_expr vars e1) e2
    | NUOp (_, e) -> vars_expr vars e
    | NWhen (e, _, x) -> vars_expr (x :: vars) e

  and vars_emerge vars e = match e.nexpr_merge_desc with
    | NMerge (x, cases) ->
      List.fold_left (fun vars (_, e) -> vars_emerge vars e) (x :: vars) cases
    | NExpr e -> vars_expr vars e

  let number_and_resolve_deps node_name (i, eqs, defmap) eq =
    let add x defmap =
      if Smap.mem x defmap then raise (MutlipleDefinitions (node_name, x));
      Smap.add x (i, eq) defmap
    in
    let defmap, vars = match eq with
      | EquSimple (x, e) -> add x defmap, vars_emerge [] e
      | EquFby (x, _, e) -> add x defmap, vars_expr [] e
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
    let (eqs, defmap) =
      let Ast_typed.Tagged (_, _, node_name) = node.n_name in
      number_and_build_defmap node_name node.n_eqs
    in
    let g =
      List.fold_left
        (fun g (i, eq, vars) ->
           List.fold_left (fun g x -> Graph.add_edge g (i, eq) (Smap.find x defmap)) g vars)
        Graph.empty
        eqs
    in
    let eqs = Graph.topo_sort g in
    let node = NNode { node with n_eqs = eqs } in
    node :: scheduled

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
end

module Stupid = struct
  let schedule file _name = file
end
