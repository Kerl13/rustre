type ident = string
type location = Lexing.position * Lexing.position



(**
 * Parsing AST without type and clock annotations
 **)


(** Types *)
type ty =
  | TyBool
  | TyInt
  | TyReal


(** Builtin operators *)
type op =
  | OpAdd | OpSub | OpMul | OpDiv | OpMod
  | OpLt | OpLe | OpGt | OpGe
  | OpEq | OpNeq
  | OpAnd | OpOr | OpImpl
  | OpNot


(** Expressions *)
type expr = {
  expr_desc  : expr_desc ;
  expr_loc   : location
}

and expr_desc =
  | EConst of const
  | EIdent of ident
  | ETuple of expr list
  | EFby   of const * expr
  | EOp    of op * expr list
  | EApp   of ident * expr list * expr
  | EWhen  of expr * ident * ident
  | EMerge of ident * (ident * expr) list

and const =
  | CNil
  | CInt of int
  | CReal of float
  | CBool of bool


(** Programs *)
type file = node list

and node = {
  n_name   : ident ;
  n_input  : (ident * ty) list ;
  n_output : (ident * ty) list ;
  n_local  : (ident * ty) list ;
  n_eqs    : equation list ;
  n_loc    : location
}

and equation = {
  eq_pat  : pattern ;
  eq_expr : expr
}

and pattern = {
  pat_desc : pattern_desc ;
  pat_loc  : location
}

and pattern_desc =
  | PIdent of ident
  | PTuple of pattern_desc list



(**
 * Pretty printer
 **)

let fprintf = Format.fprintf

let rec pp_list sep pp ppf = function
  | [] -> fprintf ppf ""
  | [x] -> fprintf ppf "%a" pp x
  | x :: xs -> fprintf ppf "%a%s%a" pp x sep (pp_list sep pp) xs

let pp_const ppf = function
  | CNil -> fprintf ppf "nil"
  | CInt n -> fprintf ppf "%d" n
  | CReal f -> fprintf ppf "%f" f
  | CBool b -> fprintf ppf "%B" b

let pp_op ppf = function
  | OpAdd -> fprintf ppf "+"
  | OpSub -> fprintf ppf "-"
  | OpMul -> fprintf ppf "*"
  | OpDiv -> fprintf ppf "/"
  | OpMod -> fprintf ppf "mod"
  | OpLt -> fprintf ppf "<"
  | OpLe -> fprintf ppf "<="
  | OpGt -> fprintf ppf ">"
  | OpGe -> fprintf ppf ">="
  | OpEq -> fprintf ppf "="
  | OpNeq -> fprintf ppf "<>"
  | OpAnd -> fprintf ppf "and"
  | OpOr -> fprintf ppf "or"
  | OpImpl -> fprintf ppf "=>"
  | OpNot -> fprintf ppf "not"

let pp_expr =
  let rec pp ppf e = pp_desc ppf e.expr_desc
  and pp_desc ppf = function
    | EConst c -> fprintf ppf "%a" pp_const c
    | EIdent i -> fprintf ppf "%s" i
    | ETuple es -> fprintf ppf "%a" (pp_list ", " pp) es
    | EFby (v, e) -> fprintf ppf "(%a fby %a)" pp_const v pp e
    | EOp (op, [e1; e2]) -> fprintf ppf "(%a %a %a)" pp e1 pp_op op pp e2
    | EOp (OpNot, [e]) -> fprintf ppf "(%a %a)" pp_op OpNot pp e
    | EOp _ -> assert false
    | EApp (f, args, ev) -> fprintf ppf "(%s(%a) every %a)" f (pp_list ", " pp) args pp ev
    | EWhen (e, c, x) -> fprintf ppf "(%a when %s(%s))" pp e c x
    | EMerge (x, clauses) -> fprintf ppf "(merge %s %a)" x (pp_list " " pp_clause) clauses
  and pp_clause ppf (c, e) = fprintf ppf "(%s -> %a)" c pp e
  in pp

let rec pp_pat ppf = function
  | PIdent i -> fprintf ppf "%s" i
  | PTuple pats -> fprintf ppf "(%a)" (pp_list ", " pp_pat) pats
let pp_pat ppf p = pp_pat ppf p.pat_desc

let pp_ty ppf = function
  | TyBool -> fprintf ppf "bool"
  | TyInt -> fprintf ppf "int"
  | TyReal -> fprintf ppf "real"

let pp_equation ppf eq =
  fprintf ppf "%a = %a" pp_pat eq.eq_pat pp_expr eq.eq_expr

let pp_node ppf n =
  let pp_arg ppf (id, ty) = fprintf ppf "%s: %a" id pp_ty ty in
  let pp_equation ppf eq = fprintf ppf "  %a" pp_equation eq in
  fprintf ppf "node %s(%a) = (%a)\nwith var %a in\n%a"
    n.n_name
    (pp_list "; " pp_arg) n.n_input
    (pp_list "; " pp_arg) n.n_output
    (pp_list "; " pp_arg) n.n_local
    (pp_list ";\n" pp_equation) n.n_eqs

let pp_file ppf f = fprintf ppf "%a" (pp_list "\n\n" pp_node) f
