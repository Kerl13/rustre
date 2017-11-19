open Ast_object
open Ast_normalized

let obc_varlist vl =
  let rec aux: type a. a Ast_typed.var_list -> var_list = function
    | Ast_typed.VIdent(a, ty) -> [a, Sty (Normalization.sty_for_ty ty)]
    | Ast_typed.VEmpty -> []
    | Ast_typed.VTuple(a, ty, b) ->
      (a, Sty (Normalization.sty_for_ty ty)) :: aux b
  in aux vl |> List.rev

let rec is_in: type a. a Ast_typed.var_list -> a Ast_typed.var_ident -> bool = fun vl v -> match vl with
  | Ast_typed.VIdent(v', _) -> v = v'
  | Ast_typed.VEmpty -> false
  | Ast_typed.VTuple(v', _, b) -> v = v' || is_in b v

type nl = Ast_clocked.node_local

let obc_const: type a. a Ast_typed.const -> a oconst = fun c ->
  match c with
  | Ast_typed.CNil -> assert false
  | Ast_typed.CBool a -> CBool a
  | Ast_typed.CInt a -> CInt a
  | Ast_typed.CReal a -> CReal a

let rec obc_expr_merge: type a. nl -> a nexpr_merge -> a oexpr = fun nl expr ->
  match expr.nexpr_merge_desc with
  | NExpr a -> obc_expr nl a

and obc_expr: type a. nl -> a nexpr -> a oexpr = fun nl expr ->
  let (Ast_clocked.NodeLocal nl_) = nl in
  match expr.nexpr_desc with
  | Ast_normalized.NConst c ->
    let oc = obc_const c in
    EConst oc
  | Ast_normalized.NIdent v ->
    if is_in nl_ v then
      EVar (State v)
    else EVar (Var v)
  | Ast_normalized.NBOp (op,e1,e2) ->
    EBOp(op, obc_expr nl e1, obc_expr nl e2)
  | Ast_normalized.NUOp (op, e1) ->
    EUOp(op, obc_expr nl e1)
  | Ast_normalized.NWhen (_,_,_) -> assert false

let obc_eq (Ast_clocked.NodeLocal local) (instances, s) = function
  | EquSimple(v, expr_merge) ->
    let v = if is_in local v then State v else Var v in
    instances,
    (SSeq (SAssign { n = v; expr = obc_expr_merge (Ast_clocked.NodeLocal local) expr_merge; }, s))
  | EquFby(pat, const, expr) -> instances, s
  | EquApp(pat, id, vl, _) ->
  let Ast_typed.Tagged(args_in, args_out, machine_id) = id in
  let args = obc_varlist vl in
  let args = List.map (fun (v, _) -> if is_in local v then State v else Var v) args in
  let res = obc_varlist pat.Ast_clocked.pat_desc in
  let res = List.map (fun (v, _) -> if is_in local v then State v else Var v) res in
  instances,
  (SSeq ((SCall(args, machine_id, res)), s))

let obc_node (NNode desc) =
  let instances, step = List.fold_left (obc_eq desc.n_local) ([], SSkip) desc.n_eqs in
  { memory =
      (let Ast_clocked.NodeLocal nl = desc.n_local in
       obc_varlist nl);
    name = (let Ast_typed.Tagged(_, _, n) = desc.n_name in n);
    instances = instances;
    reset = SSkip;
    step = obc_varlist desc.n_input,
           step; }

let from_normalized file =
  List.map obc_node file
