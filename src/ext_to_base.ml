(**
 * Translation from Ast_ext to Ast_parsing, assuming every extra feature
 * has been removed
 **)

open Ast_parsing

let rec tr_pat_desc = function
  | Ast_ext.PIdent x -> PIdent x
  | Ast_ext.PTuple l -> PTuple (List.map tr_pat_desc l)

let tr_pat p = {
  pat_desc = tr_pat_desc p.Ast_ext.pat_desc ;
  pat_loc = p.Ast_ext.pat_loc
}

let rec tr_equation eq = match eq.Ast_ext.eq_desc with
  | Ast_ext.EEq (pat, e) -> {eq_pat = tr_pat pat ; eq_expr = tr_expr e}
  | _ -> assert false

and tr_expr e = {
  expr_desc = tr_expr_desc e.Ast_ext.expr_desc ;
  expr_loc = e.Ast_ext.expr_loc
}

and tr_expr_desc = function
  | Ast_ext.EConst x -> EConst x
  | Ast_ext.EIdent x -> EIdent x
  | Ast_ext.ETuple l -> ETuple (List.map tr_expr l)
  | Ast_ext.EFby (c, e) -> EFby (c, tr_expr e)
  | Ast_ext.EOp (o, l) -> EOp (o, List.map tr_expr l)
  | Ast_ext.EApp (x, l, e) -> EApp (x, List.map tr_expr l, tr_expr e)
  | Ast_ext.EWhen (e, x, y) -> EWhen (tr_expr e, x, y)
  | Ast_ext.EMerge (x, l) -> EMerge (x, List.map (fun (a, b) -> (a, tr_expr b) ) l )
  | _ -> assert false


let tr_var_decl vd = vd.Ast_ext.v_name, vd.Ast_ext.v_type (*XXX add checks*)

let tr_node n = {
  n_name = n.Ast_ext.n_name ;
  n_input = n.Ast_ext.n_input ;
  n_output = n.Ast_ext.n_output ;
  n_local = List.map tr_var_decl n.Ast_ext.n_local ;
  n_eqs = List.map tr_equation n.Ast_ext.n_eqs ;
  n_loc = n.Ast_ext.n_loc
}

let tr_file f = {
  f_typedefs = f.Ast_ext.f_typedefs ;
  f_nodes = List.map tr_node f.Ast_ext.f_nodes
}
