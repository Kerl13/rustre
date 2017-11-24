open Ast_clocked

module type Clocking = sig
  val clock_file: Ast_typed.file -> Ast_clocked.file
end
(*
 * /!\ This is not done yet...
 *)
module W = struct
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

  exception ClockUnificationError of ck * ck
  let unification_error c1 c2 = raise (ClockUnificationError (canon c1, canon c2))

  let rec occur v c = match head c with
    | CBase -> false
    | COn (c, _, _) -> occur v c
    | CVar v' -> V.equal v v'

  let rec unify c1 c2 = match head c1, head c2 with
    | CBase, CBase -> ()
    | COn (c1, dc1, x1), COn (c2, dc2, x2) ->
        if x1 = x2 && dc1 = dc2 (* FIXME: maybe a bit too restrictive *)
        then unify c1 c2
        else unification_error c1 c2
    | CVar v1, CVar v2 when V.equal v1 v2 -> ()
    | CVar v, c2 ->
      if occur v c2
      then unification_error c1 c2
      else v.def <- Some c2
    | c1, CVar v -> unify c2 c1
    | _, _ -> unification_error c1 c2

  let rec fvars c = match head c with
    | CBase -> Vset.empty
    | COn (c, _, _) -> fvars c
    | CVar v -> Vset.singleton v

  type schema = { vars : Vset.t ; ck : ck }

  module Smap = Map.Make(String)
  module Vmap = Map.Make(V)

  (** Environment mapping variables to clocks *)
  module Env = struct
    exception Env_not_found of string

    type t = { bindings : schema Smap.t ; fvars : Vset.t }

    let empty : t = { bindings = Smap.empty ; fvars = Vset.empty }

    let add x c (env : t) = {
      bindings = Smap.add x c env.bindings ;
      fvars = Vset.union (fvars c.ck) env.fvars
    }

    let rec subst map c = match head c with
      | CBase -> CBase
      | COn (c, dc, x) -> COn (subst map c, dc, x)
      | CVar v ->
        let v' = try Vmap.find v map with Not_found -> v in
        CVar v'

    let refresh schema =
      let fv = Vset.filter (fun v -> v.def = None) schema.vars in
      let map = Vset.fold (fun v -> Vmap.add v (V.create ())) fv Vmap.empty in
      { vars = fv ; ck = subst map schema.ck }

    let find x (env : t) =
      try refresh (Smap.find x env.bindings)
      with Not_found -> raise (Env_not_found x)
  end

  (** Environment mapping nodes to clocks *)
  module NodeEnv = struct
    type t = (ct * ct) Smap.t

    let empty : t = Smap.empty

    let add x in_ct out_ct (env : t) = Smap.add x (in_ct, out_ct) env
  end

  (* Clock inferrence *****************************************************)
    (*
  let rec var_list_fold : type a b. (a -> b Ast_typed.var_ident -> a) -> a -> b Ast_typed.var_list -> a
    = fun f acc v_list -> match v_list with
      | Ast_typed.VIdent (x, _) -> f acc x
      | Ast_typed.VEmpty -> acc
      | Ast_typed.VTuple (x, _, v_list) -> var_list_fold f (f acc x) v_list
*)
  let clock_node_desc node_env node_desc =
    let env = assert false in
    env

  let ct_from_varlist env v_list =
    Ast_typed_utils.var_list_fold (fun ct x -> (Env.find x env).ck :: ct) [] v_list

  let clock_node (node_env, clocked_nodes) (Ast_typed.Node n) =
    let env, clocked_node = clock_node_desc node_env n in
    let in_clocks = ct_from_varlist env n.Ast_typed.n_input in
    let out_clocks = ct_from_varlist env n.Ast_typed.n_output in
    let Ast_typed.Tagged (_, _, node_name) = n.Ast_typed.n_name in
    let node_env = NodeEnv.add node_name in_clocks out_clocks node_env in
    node_env, clocked_node :: clocked_nodes

  let clock_file file =
    let (_, file) = List.fold_left clock_node (NodeEnv.empty, []) file in
    List.rev file
end

module Stupid = struct
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

  (** TODO: something more involved **)
  let clock_compatible (c1:ct) (c2: ct) = (c1 = c2)

  let is_compatible (env:env) (s:Ast_parsing.ident) (cl:ct) : bool =
    let current_val = Hashtbl.find env s in
    match current_val with
    | None -> Hashtbl.replace env s (Some cl); true
    | Some sc -> clock_compatible sc cl


  let clock_node_desc : type a b. (a, b) Ast_typed.node_desc -> (a, b) Ast_clocked.node_desc = fun node ->
    let n_name = node.Ast_typed.n_name
    and n_input = node.Ast_typed.n_input
    and n_output = node.Ast_typed.n_output
    and n_local = node.Ast_typed.n_local;
    and n_loc = node.Ast_typed.n_loc in

    let c_env = create_base_env node in
    let n_eqs = List.map (clock_eq c_env) node.Ast_typed.n_eqs in
    { n_name=n_name; n_input=n_input; n_output=n_output; n_local=n_local; n_loc=n_loc; n_eqs=n_eqs }

  let clock_node (Ast_typed.Node e:Ast_typed.node) : node =
    Node (clock_node_desc e)



  let clock_file (f:Ast_typed.file) : file = List.map clock_node f
end
