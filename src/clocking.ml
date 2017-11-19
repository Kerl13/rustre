open Ast_clocked

module type Clocking = sig
  val clock_file: Ast_typed.file -> Ast_clocked.file
end
(*
 * /!\ This is not done yet...
 *)

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
      CConst c, CSingle CBase
    | Ast_typed.EIdent v ->
      CIdent v, CSingle CBase
    | Ast_typed.EFby (c, e) ->
      CFby (c, clock_expr env e), CSingle CBase
    | Ast_typed.EBOp (b, e1, e2) ->
      CBOp (b, clock_expr env e1, clock_expr env e2), CSingle CBase
    | Ast_typed.EUOp (b, e) ->
      CUOp (b, clock_expr env e), CSingle CBase
    | Ast_typed.EApp (f, args, ev) ->
      CApp (f, clock_expr_list env args, clock_expr env ev), CSingle CBase
    | Ast_typed.EWhen (e, c, x) ->
      CWhen (clock_expr env e, c, x), CSingle CBase
    | Ast_typed.EMerge (x, clauses) ->
      CMerge (x, List.map (fun (x, e) -> (x, clock_expr env e)) clauses), CSingle CBase

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
    let _ = List.map (fun x -> Hashtbl.add env x (Some (CSingle CBase))) var_in in
    (* To create H_q *)
    let _ = List.map (fun x -> Hashtbl.add env x (Some (CSingle CBase))) var_out in
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
