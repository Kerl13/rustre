open Ast_normalized

module type Scheduling = sig
  exception Error of Ast_typed.location * string
  val schedule : Ast_normalized.nfile -> string -> Ast_normalized.nfile
end

module Check = struct
  module Sset = Set.Make(String)

  exception UnboundVar of string
  exception UnboundNode of string


  let assert_in x s = if not (Sset.mem x s) then raise (UnboundVar x)
  let assert_in_node x s = if not (Sset.mem x s) then raise (UnboundNode x)

  let rec check_expr : type a. Sset.t -> Sset.t -> a nexpr -> unit
    = fun seen_nodes seen_vars e -> match e.nexpr_desc with
      | Ast_normalized.NConst _ -> ()
      | Ast_normalized.NIdent x ->
        assert_in x seen_vars
      | Ast_normalized.NBOp (_, e1, e2) ->
        check_expr seen_nodes seen_vars e1 ;
        check_expr seen_nodes seen_vars e2
      | Ast_normalized.NUOp (_, e) ->
        check_expr seen_nodes seen_vars e
      | Ast_normalized.NWhen (e, _, x) ->
        check_expr seen_nodes seen_vars e ;
        assert_in x seen_vars

  let rec check_mexpr seen_nodes seen_vars me = match me.nexpr_merge_desc with
    | Ast_normalized.NMerge (x, clauses) ->
      assert_in x seen_vars ;
      List.iter (fun (_, e) -> check_mexpr seen_nodes seen_vars e) clauses
    | Ast_normalized.NExpr e -> check_expr seen_nodes seen_vars e

  let check_eq seen_nodes seen_vars = function
    | EquSimple (x, e) ->
      check_mexpr seen_nodes seen_vars e ;
      Sset.add x seen_vars
    | EquFby (x, _, _) ->
      Sset.add x seen_vars
    | EquApp (pat, Ast_typed.Tagged (_, _, f), args, every, _) ->
      check_expr seen_nodes seen_vars every ;
      assert_in_node f seen_nodes ;
      Ast_typed_utils.var_list_fold (fun () x -> assert_in x seen_vars) () args ;
      Ast_typed_utils.var_list_fold (fun seen x -> Sset.add x seen) seen_vars pat.Ast_typed.pat_desc

  let check_node seen_nodes (NNode n) =
    let seen_vars =
      let add s x = Sset.add x s in
      Ast_typed_utils.var_list_fold add Sset.empty n.n_input
    in
    let Ast_typed.Tagged (_, _, name) = n.n_name in
    let () = begin try
        let _ = List.fold_left (check_eq seen_nodes) seen_vars n.n_eqs in
        ()
        (* No problem found *)
      with
      | UnboundVar x ->
        Format.eprintf "Internal error:\n" ;
        Format.eprintf "Scheduling sanity checks failed on variable %s in node %s\n" x name ;
        Format.eprintf "Continuing anyway… May the force be with you!@."
      | UnboundNode f ->
        Format.eprintf "Internal error:\n" ;
        Format.eprintf "Scheduling sanity checks failed at call to node %s in node %s\n" f name ;
        Format.eprintf "Continuing anyway… May the force be with you!@."
    end in
    Sset.add name seen_nodes

  let check file main =
    let _ = List.fold_left check_node Sset.empty file.nf_nodes in
    begin match List.rev file.nf_nodes with
    | [] -> assert false
    | n :: _ ->
      let NNode { n_name = Ast_typed.Tagged (_, _, name) ; _ } = n in
      assert (name = main)
    end ;
    (* No problem found *)
    ()
end

module Simple = struct
  (** Public exception *)
  exception Error of Ast_typed.location * string

  (** Private exceptions *)
  exception Not_a_node of string
  exception MultipleDefinitions of string * string * Ast_typed.location
  exception CausalityError of string * nequation list * Ast_typed.location
  exception NoDefinition of string * string * Ast_typed.location

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
        let leaves = EqMap.filter (fun _ -> (=) []) g in
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

  let eq_loc = function
    | EquSimple (_, e) -> e.nexpr_merge_loc
    | EquFby (_, _, e) -> e.nexpr_loc
    | EquApp (_, _, _, e, _) -> e.nexpr_loc

  let number_and_resolve_deps node_name (i, eqs, defmap) eq =
    let add x defmap =
      if Smap.mem x defmap then raise (MultipleDefinitions (node_name, x, eq_loc eq));
      Smap.add x (i, eq) defmap
    in
    let defmap, vars = match eq with
      | EquSimple (x, e) -> add x defmap, vars_emerge [] e
      | EquFby (x, _, _) -> add x defmap, []
      | EquApp (pat, _, arg, every, _) ->
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
      try Graph.add_edge g ieq (Smap.find x defmap)
      with Not_found ->
        (* If the var that triggers this exception is an input, that's normal.
           Otherwise, it means that one stream is undefined *)
        if Ast_typed_utils.var_list_exists ((=) x) node.n_input
        then g
        else raise (NoDefinition (x, node_name, node.n_loc))
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
      with Graph.Cycle g ->
        raise (CausalityError (node_name, Graph.keys g, node.n_loc))
    in
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
    try
      let file = schedule file main in
      Check.check file main ;
      file
    with
    | Not_a_node name ->
      let message = Format.sprintf "Unbound node %s" name in
      let pos = Lexing.dummy_pos in
      raise (Error ((pos, pos), message))
    | MultipleDefinitions (node_name, x, loc) ->
      let message = Format.sprintf "Variable %s is defined multiple times in node %s" x node_name in
      raise (Error (loc, message))
    | CausalityError (node_name, eqs, loc) ->
      let message = Format.asprintf
        "Cyclic dependencies in node %s between the following equations:\n  %a"
        node_name (Pp_utils.pp_list "\n  " pp_eq) eqs
      in
      raise (Error (loc, message))
    | NoDefinition (x, node, loc) ->
      let message = Format.sprintf "Variable %s is never defined in node %s" x node in
      raise (Error (loc, message))
end

module Stupid = struct
  exception Error of Ast_typed.location * string
  let schedule file _name = file
end
