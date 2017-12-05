open Ast_object
open Ast_normalized

let new_instance =
  let counter = ref 0 in
  fun () ->
    incr counter;
    Format.sprintf "inst%d" !counter

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

let obc_const: type a. a Ast_typed.ty -> a Ast_typed.const -> a oconst = fun ty c ->
  match c with
  | Ast_typed.CNil -> begin match ty with
      | Ast_typed.TyBool -> CBool true
      | Ast_typed.TyNum (Ast_typed.TyZ) -> CInt 42
      | Ast_typed.TyNum (Ast_typed.TyReal) -> CReal 0.
      | Ast_typed.TyEnum (_, dcs) -> CDataCons (List.hd dcs)
    end
  | Ast_typed.CBool a -> CBool a
  | Ast_typed.CInt a -> CInt a
  | Ast_typed.CReal a -> CReal a
  | Ast_typed.CDataCons dc -> CDataCons dc

let rec obc_expr_merge: type a. nl -> ident -> a nexpr_merge -> ostatement = fun nl v expr ->
  match expr.nexpr_merge_desc with
  | NExpr a -> SAssign { n = v; expr = obc_expr nl a; }
  | NMerge(id, cases) ->
    let Ast_typed.NodeLocal local = nl in
    let id = if is_in local id then Loc id else Var id in
    SCase(EVar id, List.map (fun (constructor, expr) ->
        constructor, obc_expr_merge nl v expr) cases)

and obc_expr: type a. nl -> a nexpr -> a oexpr = fun nl expr ->
  let (Ast_typed.NodeLocal nl_) = nl in
  match expr.nexpr_desc with
  | Ast_normalized.NConst c ->
    let oc = obc_const expr.nexpr_type c in
    EConst oc
  | Ast_normalized.NIdent v ->
    if is_in nl_ v then
      EVar (Loc v)
    else EVar (Var v)
  | Ast_normalized.NBOp (op,e1,e2) ->
    EBOp(op, obc_expr nl e1, obc_expr nl e2)
  | Ast_normalized.NUOp (op, e1) ->
    EUOp(op, obc_expr nl e1)
  | Ast_normalized.NWhen (e,_,_) ->
    obc_expr nl e

let obc_eq (Ast_typed.NodeLocal local) (instances, s, end_os) = function
  | EquSimple(v, expr_merge) ->
    let v = if is_in local v then Loc v else Var v in
    instances,
    (SSeq (s, obc_expr_merge (Ast_typed.NodeLocal local) v expr_merge)),
    end_os
  | EquFby(v, c, expr_merge) ->
    (* apart from the scheduling, equfby is the same thing as equsimple *)
    let state, instances = instances in
    ((((v:var_id), Sty expr_merge.nexpr_type), Const (obc_const expr_merge.nexpr_type c)) :: state, instances),
    s,
    (SSeq (end_os, SAssign { n = State v; expr = obc_expr (Ast_typed.NodeLocal local) expr_merge; }))
  | EquApp(pat, id, vl, every) ->
    let Ast_typed.Tagged(_, _, machine_id) = id in
    let args = obc_varlist vl in
    let args = List.map (fun (v, _) -> if is_in local v then Loc v else Var v) args in
    let res = obc_varlist pat.Ast_typed.pat_desc in
    let res = List.map (fun (v, _) -> if is_in local v then Loc v else Var v) res in
    let i = new_instance () in
    let reset = SCase (obc_expr (Ast_typed.NodeLocal local) every, ["True", SReset (machine_id, i); "False", SSkip]) in
    let state, instances = instances in
    (state, (i, machine_id)::instances),
    (SSeq (s, SSeq (reset, SCall(args, i, machine_id, res)))),
    end_os

let obc_node (NNode desc) =
  let (state, instances), step, end_os = List.fold_left (obc_eq desc.n_local) (([], []), SSkip, SSkip) desc.n_eqs in
  let step = List.fold_left (fun a (((b:var_id), _), _) ->
      let Ast_typed.NodeLocal nl = desc.n_local in
      let v = if is_in nl b then
          (Loc b) else (Var b)
      in
      SSeq (SAssign { n = v;  expr = EVar (State b); }, a)) step state in
  let step = SSeq(step, end_os) in
  let reset_var = List.fold_left (fun a (((b:var_id), _), (Const c)) ->
                      SSeq (SAssign { n = State b;  expr = EConst c; }, a)) SSkip state in
  let reset = List.fold_left (fun a (i, m) ->
                  SSeq (SReset (m, i), a)) reset_var instances in
  { memory = List.map fst state;
    name = (let Ast_typed.Tagged(_, _, n) = desc.n_name in n);
    instances = instances;
    reset = reset;
    step = obc_varlist desc.n_input,
           (let Ast_typed.NodeLocal nl = desc.n_local in
            obc_varlist nl),
           obc_varlist desc.n_output,
           step; }

let from_normalized file =
  let machines = List.map obc_node file.Ast_normalized.nf_nodes in
  { objf_typedefs = file.Ast_normalized.nf_typedefs ; objf_machines = machines }