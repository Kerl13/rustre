open Ast_clocked
type location = Ast_typed.location

module type Clocking = sig
  exception Error of location * string
  val clock_file: Ast_typed.file -> string -> Ast_clocked.file
  (** We must know which is the main node *)
end

module W = struct
  exception Error of location * string

  (** W algorithm usual stuff ***********************************************)
  module V = struct
    type t = cvar
    let compare v1 v2 = Pervasives.compare v1.id v2.id
    let equal v1 v2 = v1.id = v2.id
    let create =
      let r = ref (-1) in
      fun () ->
        incr r ;
        { id = !r ; def = None }
  end

  module Vset = Set.Make(V)

  let rec head = function
    | CVar { id = _ ; def = Some c } -> head c
    | c -> c

  let rec canon c = match head c with
    | CBase -> CBase
    | COn (c, dc, x) -> COn (canon c, dc, x)
    | CVar v -> CVar v

  exception ClockUnificationError of location * ck * ck
  let unification_error loc c1 c2 = raise (ClockUnificationError (loc, canon c1, canon c2))
  exception ArityException of location * int * ct
  let arity_exception loc n ct = raise (ArityException (loc, n, List.map canon ct))

  let rec occur v c = match head c with
    | CBase -> false
    | COn (c, _, _) -> occur v c
    | CVar v' -> V.equal v v'

  let rec unify (at : location) c1 c2 = match head c1, head c2 with
    | CBase, CBase -> ()
    | COn (c1', dc1, x1), COn (c2', dc2, x2) ->
        if x1 = x2 && dc1 = dc2 (* FIXME: maybe a bit too restrictive *)
        then unify at c1' c2'
        else unification_error at c1 c2
    | CVar v1, CVar v2 when V.equal v1 v2 -> ()
    | CVar v, c2 ->
      if occur v c2
      then unification_error at c1 c2
      else v.def <- Some c2
    | c1, CVar _ -> unify at c2 c1
    | _, _ -> unification_error at c1 c2

  let rec unify_ct (at : location) ct1 ct2 = match (ct1, ct2) with
    | [], [] -> ()
    | c1 :: ct1, c2 :: ct2 -> unify at c1 c2 ; unify_ct at ct1 ct2
    | _ -> assert false

  let rec fvars c = match head c with
    | CBase -> Vset.empty
    | COn (c, _, _) -> fvars c
    | CVar v -> Vset.singleton v

  module Vmap = Map.Make(V)

  module Env = struct
    exception Expr_not_found of location * string
    exception Node_not_found of location * string

    type t = {
      bindings_expr : ck Smap.t ;
      bindings_node : (Vset.t * ct * ct) Smap.t ;
      fvars : Vset.t
    }

    let empty : t = {
      bindings_expr = Smap.empty ;
      bindings_node = Smap.empty ;
      fvars = Vset.empty
    }

    let reset_exprs (env : t) = { env with bindings_expr = Smap.empty }
    let get_exprs (env : t) = Smap.map canon env.bindings_expr

    let add_expr x ck (env : t) = {
      bindings_expr = Smap.add x ck env.bindings_expr ;
      bindings_node = env.bindings_node ;
      fvars = Vset.union (fvars ck) env.fvars
    }

    let add_node x in_ct out_ct (env : t) =
      let add_fv ck (vars, ct) =
        let vars = Vset.union vars (fvars ck) in
        (vars, ck :: ct)
      in
      let vars, in_ct = List.fold_right add_fv in_ct (Vset.empty, []) in
      let vars, out_ct = List.fold_right add_fv out_ct (vars, []) in
      { bindings_expr = env.bindings_expr ;
        bindings_node = Smap.add x (vars, in_ct, out_ct) env.bindings_node ;
        fvars = Vset.union env.fvars vars
      }

  let rec subst map c = match head c with
    | CBase -> CBase
    | COn (c, dc, x) -> COn (subst map c, dc, x)
    | CVar v ->
      let v' = try Vmap.find v map with Not_found -> v in
      CVar v'

  let find_expr (at : location) x (env : t) =
      try Smap.find x env.bindings_expr
      with Not_found -> raise (Expr_not_found (at, x))

    let refresh_node (vars, in_ct, out_ct) =
      let fv = Vset.filter (fun v -> v.def = None) vars in
      let map = Vset.fold (fun v -> Vmap.add v (V.create ())) fv Vmap.empty in
      fv, List.map (subst map) in_ct, List.map (subst map) out_ct

    let find_node (at : location) x (env : t) =
      try refresh_node (Smap.find x env.bindings_node)
      with Not_found -> raise (Node_not_found (at, x))
  end

  (* Clock inferrence *****************************************************)

  let rec cts_from_expr_list : type a. ct list -> a cexpr_list -> ct list
    = fun cts es -> match es with
    | CLNil -> cts
    | CLSing e -> e.texpr_clock :: cts
    | CLCons (e, es) -> cts_from_expr_list (e.texpr_clock :: cts) es
  let cts_from_expr_list es = cts_from_expr_list [] es |> List.rev

  let rec clock_expr : type a. Env.t -> a Ast_typed.expr -> a cexpr
    = fun env expr ->
      let (desc : a cexpr_desc), ct =
        let eloc = expr.Ast_typed.texpr_loc in
        begin match expr.Ast_typed.texpr_desc with
        | Ast_typed.EConst c -> CConst c, [CVar (V.create ())]
        | Ast_typed.EIdent x -> CIdent x, [Env.find_expr eloc x env]
        | Ast_typed.EFby (c, e) ->
          let e = clock_expr env e in
          CFby (c, e), e.texpr_clock
        | Ast_typed.EBOp (op, e1, e2) ->
          let e1 = clock_expr env e1 in
          let e2 = clock_expr env e2 in
          unify_ct e2.texpr_loc e1.texpr_clock e2.texpr_clock ;
          CBOp (op, e1, e2), e1.texpr_clock
        | Ast_typed.EUOp (op, e) ->
          let e = clock_expr env e in
          CUOp (op, e), e.texpr_clock
        | Ast_typed.EApp (f, arg, every) ->
          let Ast_typed.Tagged (_, _, f_id) = f in
          let loc = expr.Ast_typed.texpr_loc in
          let _, ct1, ct2 = Env.find_node loc f_id env in
          let arg = clock_expr_list env arg in
          let cts = cts_from_expr_list arg in
          List.iter2 (unify_ct loc) (List.map (fun c -> [c]) ct1) cts;
          let every = clock_expr env every in
          unify_ct every.texpr_loc every.texpr_clock [CBase];
          CApp (f, arg, every), ct2
        | Ast_typed.EWhen (e, c, x) ->
          let e = clock_expr env e in
          let ck_e = match e.texpr_clock with
            | [ck_e] -> ck_e
            | ct -> arity_exception e.texpr_loc 1 ct
          in
          let ck_x = Env.find_expr eloc x env in
          unify e.texpr_loc ck_e ck_x;
          CWhen (e, c, x), [COn (ck_e, c, x)]
        | Ast_typed.EMerge (x, cases) ->
          let ck = Env.find_expr eloc x env in
          let cases = clock_match_cases ck x env cases in
          CMerge (x, cases), [ck]
        end in
    { texpr_desc = desc ;
      texpr_type = expr.Ast_typed.texpr_type ;
      texpr_clock = ct ;
      texpr_loc = expr.Ast_typed.texpr_loc }

  and clock_match_cases : type a. ck -> string -> Env.t -> (string * a Ast_typed.expr) list -> (string * a cexpr) list
    = fun ck x env cases ->
    let rec clock_cases clocked_cases = function
      | [] -> clocked_cases
      | (dc, e) :: cases ->
        let e = clock_expr env e in
        let ck_e = match e.texpr_clock with
          | [ck] -> ck
          | ct -> arity_exception e.texpr_loc 1 ct
        in
        unify e.texpr_loc ck_e (COn (ck, dc, x));
        clock_cases ((dc, e) :: clocked_cases) cases
    in
    List.rev (clock_cases [] cases)

  and clock_expr_list : type a. Env.t -> a Ast_typed.expr_list -> a cexpr_list
    = fun env e_list -> match e_list with
    | Ast_typed.ELNil -> CLNil
    | Ast_typed.ELSing e -> CLSing (clock_expr env e)
    | Ast_typed.ELCons (e, es) ->
      let e = clock_expr env e in
      let es = clock_expr_list env es in
      CLCons (e, es)

  let clock_equation env eq clocked_eqs =
    let Ast_typed.Equ (pat, expr) = eq in
    let expr = clock_expr env expr in
    let loc = expr.texpr_loc in
    let unify_ct ct x = match ct with
      | ck :: ct -> unify loc ck (Env.find_expr loc x env) ;
        ct
      | _ -> assert false (* should not happen *)
    in
    let _ = Ast_typed_utils.var_list_fold unify_ct expr.texpr_clock pat.Ast_typed.pat_desc in
    Equ (pat, expr) :: clocked_eqs

  let clock_equations env eqs =
    List.fold_right (clock_equation env) eqs []

  let ct_from_varlist loc env v_list =
    Ast_typed_utils.var_list_fold (fun ct x -> Env.find_expr loc x env :: ct) [] v_list
    |> List.rev

  let env_from_varlist new_clock env v_list =
    Ast_typed_utils.var_list_fold
      (fun env x -> Env.add_expr x (new_clock ()) env)
      env
      v_list

  let env_from_varlist_fresh env vl = env_from_varlist (fun () -> CVar (V.create ())) env vl
  let env_from_varlist_base env vl = env_from_varlist (fun () -> CBase) env vl

  let clock_node main (env, clocked_nodes) (Ast_typed.Node n) =
    let Ast_typed.Tagged (_, _, node_name) = n.Ast_typed.n_name in
    let Ast_typed.NodeLocal locals = n.Ast_typed.n_local in
    let inputs = n.Ast_typed.n_input in
    let outputs = n.Ast_typed.n_output in
    let loc = n.Ast_typed.n_loc in

    let env = Env.reset_exprs env in
    let env =
      if node_name = main
      then env_from_varlist_base env inputs
      else env_from_varlist_fresh env inputs
    in
    let env = env_from_varlist_fresh env locals in
    let env = env_from_varlist_fresh env outputs in

    let clocked_eqs = clock_equations env n.Ast_typed.n_eqs in
    let clocked_node = Node {
      n_name = n.Ast_typed.n_name ;
      n_input = inputs ;
      n_output = outputs ;
      n_local = n.Ast_typed.n_local ;
      n_eqs = clocked_eqs ;
      n_loc = loc ;
      n_clocks = Env.get_exprs env
    } in

    let in_clocks = ct_from_varlist loc env inputs in
    let out_clocks = ct_from_varlist loc env outputs in
    let env = Env.add_node node_name in_clocks out_clocks env in
    env, clocked_node :: clocked_nodes

  let clock_file file main =
    let (_, nodes) = List.fold_left (clock_node main) (Env.empty, []) file.Ast_typed.tf_nodes in
    { cf_typedefs = file.Ast_typed.tf_typedefs ; cf_nodes = List.rev nodes }
  let clock_file file main =
    try
      clock_file file main
    with
    | ClockUnificationError (loc, c1, c2) ->
      let pp_ck = pp_ck (symbol_map ()) in
      let message = Format.asprintf "Incompatible clocks: [%a] and [%a]" pp_ck c1 pp_ck c2 in
      raise (Error (loc, message))
    | ArityException (loc, n, ct) ->
      let pp_ck = pp_ck (symbol_map ()) in
      let message = Format.asprintf
          "Composite clock [%a] is expected to have arity %d but has arity %d"
          (Pp_utils.pp_list ", " pp_ck) ct n (List.length ct) in
      raise (Error (loc, message))
    | Env.Node_not_found (loc, name) ->
      let message = Format.sprintf "Unbound node %s" name in
      raise (Error (loc, message))
    | Env.Expr_not_found (loc, name) ->
      let message = Format.sprintf "Unbound variable %s" name in
      raise (Error (loc, message))
end

module Stupid = struct
  exception Error of Ast_typed.location * string

  type env = (string, ct option) Hashtbl.t

  let rec clock_expr : type a. env -> a Ast_typed.expr -> a cexpr = fun env ->
    fun exp ->
    let texpr_desc, texpr_clock = clock_expr_desc env exp.Ast_typed.texpr_desc in
    { texpr_desc=texpr_desc; texpr_clock=texpr_clock; texpr_type=exp.Ast_typed.texpr_type; texpr_loc=exp.Ast_typed.texpr_loc }

and clock_expr_list: type a. env -> a Ast_typed.expr_list -> a cexpr_list = fun env ->
function
| Ast_typed.ELNil -> Ast_clocked.CLNil
| Ast_typed.ELSing a -> Ast_clocked.CLSing (clock_expr env a)
| Ast_typed.ELCons(a, b) -> Ast_clocked.CLCons (clock_expr env a, clock_expr_list env b)
  and clock_expr_desc : type a. env -> a Ast_typed.expr_desc -> (a cexpr_desc * ct) = fun env -> fun exp ->
    match exp with
    | Ast_typed.EConst c ->
      CConst c, [CBase]
    | Ast_typed.EIdent v ->
      CIdent v, [CBase]
    | Ast_typed.EFby (c, e) ->
      CFby (c, clock_expr env e), [CBase]
    | Ast_typed.EBOp (b, e1, e2) ->
      CBOp (b, clock_expr env e1, clock_expr env e2), [CBase]
    | Ast_typed.EUOp (b, e) ->
      CUOp (b, clock_expr env e), [CBase]
    | Ast_typed.EApp (f, args, ev) ->
      CApp (f, clock_expr_list env args, clock_expr env ev), [CBase]
    | Ast_typed.EWhen (e, c, x) ->
      CWhen (clock_expr env e, c, x), [CBase]
    | Ast_typed.EMerge (x, clauses) ->
      CMerge (x, List.map (fun (x, e) -> (x, clock_expr env e)) clauses), [CBase]

  let clock_eq: env -> Ast_typed.equation -> Ast_clocked.equation = fun env ->
    fun (Ast_typed.Equ (a, b)) ->
    let c_pat = a in
    let c_expr = clock_expr env b in
    Equ (c_pat, c_expr)



  let rec untag : type a.  a Ast_typed.var_list -> Ast_parsing.ident list = fun l -> match l with
    | Ast_typed.VIdent (x, _) -> [x]
    | Ast_typed.VEmpty -> []
    | Ast_typed.VTuple  (x, _, l') -> x::(untag l')

  let create_base_env (n:('a, 'b) Ast_typed.node_desc) : env =
    let Ast_typed.NodeLocal local_vars = n.Ast_typed.n_local in
    let var_in = untag n.Ast_typed.n_input in
    let var_out = untag n.Ast_typed.n_output in
    let var_local = untag local_vars in
    let env =  Hashtbl.create (List.length var_in + List.length var_out + List.length var_local) in
    (* To create H_p *)
    let _ = List.map (fun x -> Hashtbl.add env x (Some [CBase])) var_in in
    (* To create H_q *)
    let _ = List.map (fun x -> Hashtbl.add env x (Some [CBase])) var_out in
    (* To create H_r (not instantiated yet *)
    let _ = List.map (fun x -> Hashtbl.add env x None) var_local in
    env

  let clock_node_desc : type a b. (a, b) Ast_typed.node_desc -> (a, b) Ast_clocked.node_desc = fun node ->
    let n_name = node.Ast_typed.n_name
    and n_input = node.Ast_typed.n_input
    and n_output = node.Ast_typed.n_output
    and n_local = node.Ast_typed.n_local;
    and n_loc = node.Ast_typed.n_loc in

    let c_env = create_base_env node in
    let get = function
      | Some [ck] -> ck
      | _ -> CBase
    in
    let map = Hashtbl.fold (fun x c map -> Smap.add x (get c) map) c_env Smap.empty in
    let n_eqs = List.map (clock_eq c_env) node.Ast_typed.n_eqs in
    { n_name = n_name;
      n_input = n_input;
      n_output = n_output;
      n_local = n_local;
      n_clocks = map ;
      n_loc = n_loc; n_eqs = n_eqs }

  let clock_node (Ast_typed.Node e:Ast_typed.node) : node =
    Node (clock_node_desc e)

  let clock_file (f:Ast_typed.file) (_ : string) : file = {
    cf_typedefs = f.Ast_typed.tf_typedefs ;
    cf_nodes = List.map clock_node f.Ast_typed.tf_nodes
  }
end
