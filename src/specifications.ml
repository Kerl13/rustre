open Pp_utils
open Ast_parsing

let fprintf = Format.fprintf

let spec_const ppf c =
  match c with
  | CNil -> assert false
  | CInt i -> fprintf ppf "%d" i
  | CReal f -> fprintf ppf "%f" f
  | CBool b -> fprintf ppf "%b" b
  | CDataCons i -> fprintf ppf "%s" i

let spec_op ppf o =
  match o with
  | OpAdd -> fprintf ppf "splus"
  | OpSub -> fprintf ppf "ssub"
  | OpAnd -> fprintf ppf "sand"
  | OpOr -> fprintf ppf "sor"
  | OpEq -> fprintf ppf "seq"
  | _ -> assert false

let rec spec_expr ppf expr =
  match expr.expr_desc with
  | Ast_parsing.EConst c -> fprintf ppf "(sconst %a)" spec_const c
  | Ast_parsing.EIdent s -> fprintf ppf "%s" s
  | Ast_parsing.ETuple l -> fprintf ppf "(%a)" (pp_list ", " spec_expr) l
  | Ast_parsing.EFby (c, e) -> fprintf ppf "sfby %a %a" spec_const c spec_expr e
  | Ast_parsing.EOp (op, l) -> fprintf ppf "(%a %a)" spec_op op (pp_list " " spec_expr) l
  | Ast_parsing.EApp (a,b,_) -> assert false
  | Ast_parsing.EWhen (_,_,_) -> assert false
  | Ast_parsing.EMerge (_,_) -> assert false

let rec spec_pat_desc ppf pat =
  match pat with
  | PIdent pi ->
    fprintf ppf "%s" pi
  | PTuple l ->
    fprintf ppf "(%a)" (pp_list ", " spec_pat_desc) l

let spec_eq ppf eq =
  match eq.eq_expr.expr_desc with
  | EApp(a, b, _) ->
    fprintf ppf "spec_%s %a %a" a spec_pat_desc eq.eq_pat.pat_desc (pp_list " " spec_expr) b
  | _ ->
    fprintf ppf "%a = %a" spec_pat_desc eq.eq_pat.pat_desc spec_expr eq.eq_expr

let spec_node ppf node =
  fprintf ppf "%a" (pp_list_n "/\\ \n" spec_eq) node.n_eqs

let spec_file ppf f =
  fprintf ppf "%a" (pp_list_n "\n" spec_node) f.f_nodes
