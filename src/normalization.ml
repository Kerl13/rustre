let sty_for_const: type a. a Ast_typed.const -> a Ast_normalized.sty = fun a ->
  match a with
  | Ast_typed.CNil -> assert false
  | Ast_typed.CBool _ -> Ast_normalized.StyBool
  | Ast_typed.CInt _ -> Ast_normalized.StyNum Ast_typed.TyZ
  | Ast_typed.CReal _ ->  Ast_normalized.StyNum Ast_typed.TyReal

let sty_for_ty: type a. a Ast_typed.ty -> a Ast_normalized.sty = fun _ -> assert false

let normalize_eqs (a, Ast_clocked.NodeLocal b) (Ast_clocked.Equ (pat, expr)) =
  let open Ast_clocked in
  let { texpr_desc; texpr_type; texpr_clock; texpr_loc } = expr in
  let open Ast_normalized in
  match texpr_desc with
  | CConst c ->
    let sty = sty_for_const c in
    let nexpr = {
      nexpr_desc = NConst c;
      nexpr_type = sty;
      nexpr_clock = texpr_clock;
      nexpr_loc = texpr_loc;
    } in
    let nexpr_merge = {
      nexpr_merge_desc = NExpr nexpr;
      nexpr_merge_type = sty;
      nexpr_merge_clock = texpr_clock;
      nexpr_merge_loc = texpr_loc;
    } in
    EquSimple (pat, nexpr_merge) :: a, NodeLocal b
  | CIdent _ -> assert false
  | CPair (_, _) -> assert false
  | CFby (_, _) -> assert false
  | CBOp (op, _, _) -> assert false
  | CUOp (op, _) -> assert false
  | CApp (_, _, _) -> assert false
  | CWhen (_, _, _) -> assert false
  | CMerge (_, _) -> assert false

let normalize_node (Ast_clocked.Node desc) =
  let open Ast_clocked in
  let eqs, n_local = List.fold_left normalize_eqs ([], desc.n_local) desc.n_eqs  in
  Ast_normalized.NNode Ast_normalized.{
      n_name = desc.n_name;
      n_input = desc.n_input;
      n_output = desc.n_output;
      n_local = desc.n_local;
      n_eqs = eqs;
      n_loc = desc.n_loc;
    }


let normalize_file = List.map normalize_node
