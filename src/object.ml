open Ast_object
open Ast_normalized

let obc_varlist vl =
  let rec aux: type a. a Ast_typed.var_list -> var_list = function
    | Ast_typed.VIdent(a, ty) -> [a, Sty (ty)]
    | Ast_typed.VEmpty -> []
    | Ast_typed.VTuple(a, ty, b) ->
      (a, Sty (ty)) :: aux b
  in aux vl |> List.rev

let rec is_in: type a. a Ast_typed.var_list -> a Ast_typed.var_ident -> bool = fun vl v -> match vl with
  | Ast_typed.VIdent(v', _) -> v = v'
  | Ast_typed.VEmpty -> false
  | Ast_typed.VTuple(v', _, b) -> v = v' || is_in b v

type nl = Ast_typed.node_local

let obc_const: type a. a Ast_typed.const -> a oconst = fun c ->
  match c with
  | Ast_typed.CNil -> assert false
  | Ast_typed.CBool a -> CBool a
  | Ast_typed.CInt a -> CInt a
  | Ast_typed.CReal a -> CReal a

let rec obc_expr_merge: type a. nl -> ident -> a nexpr_merge -> ostatement = fun nl v expr ->
  match expr.nexpr_merge_desc with
  | NExpr a -> SAssign { n = v; expr = obc_expr nl a; }
  | NMerge(id, cases) ->
    let Ast_typed.NodeLocal local = nl in
    let id = if is_in local id then State id else Var id in
    SCase(id, List.map (fun (constructor, expr) ->
        constructor, obc_expr_merge nl v expr) cases)

and obc_expr: type a. nl -> a nexpr -> a oexpr = fun nl expr ->
  let (Ast_typed.NodeLocal nl_) = nl in
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
  | Ast_normalized.NWhen (e,_,_) ->
    obc_expr nl e

let obc_eq (Ast_typed.NodeLocal local) (instances, s) = function
  | EquSimple(v, expr_merge) ->
    let v = if is_in local v then State v else Var v in
    instances,
    (SSeq (obc_expr_merge (Ast_typed.NodeLocal local) v expr_merge, s))
  | EquFby(v, _, expr_merge) ->
    (* apart from the scheduling, equfby is the same thing as equsimple *)
    let v = if is_in local v then State v else assert false in
    instances,
    (SSeq (SAssign { n = v; expr = obc_expr (Ast_typed.NodeLocal local) expr_merge; }, s))
  | EquApp(pat, id, vl, _) ->
    let Ast_typed.Tagged(_, _, machine_id) = id in
    let args = obc_varlist vl in
    let args = List.map (fun (v, _) -> if is_in local v then State v else Var v) args in
    let res = obc_varlist pat.Ast_typed.pat_desc in
    let res = List.map (fun (v, _) -> if is_in local v then State v else Var v) res in
    instances,
    (SSeq ((SCall(args, machine_id, res)), s))

let obc_node (NNode desc) =
  let instances, step = List.fold_left (obc_eq desc.n_local) ([], SSkip) desc.n_eqs in
  { memory =
      (let Ast_typed.NodeLocal nl = desc.n_local in
       obc_varlist nl);
    name = (let Ast_typed.Tagged(_, _, n) = desc.n_name in n);
    instances = instances;
    reset = SSkip;
    step = obc_varlist desc.n_input,
           step; }

let from_normalized file =
  List.map obc_node file
