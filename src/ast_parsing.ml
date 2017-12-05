type ident = string
type location = Lexing.position * Lexing.position



(**
 * Parsing AST without type and clock annotations
 **)


(** Types *)
type enum = string * string list
type ty =
  | TyBool
  | TyInt
  | TyReal
  | TyEnum of enum


(** Builtin operators *)
type op =
  | OpAdd | OpSub | OpMul | OpDiv | OpMod
  | OpLt | OpLe | OpGt | OpGe
  | OpEq | OpNeq
  | OpAnd | OpOr | OpImpl
  | OpNot



type const =
  | CNil
  | CInt of int
  | CReal of float
  | CBool of bool
  | CDataCons of ident

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


(** Programs *)
type file = {
  f_typedefs : enum list ;
  f_nodes : node list
}

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


(** A visitor for expressions *)
let rec visit f state e =
  let state = f state e.expr_desc in
  match e.expr_desc with
  | EConst _ | EIdent _ -> state
  | ETuple es -> List.fold_left (visit f) state es
  | EFby (_, e) -> visit f state e
  | EOp (_, es) -> List.fold_left (visit f) state es
  | EApp (_, es, every) -> List.fold_left (visit f) (visit f state every) es
  | EWhen (e, _, _) -> visit f state e
  | EMerge (_, clauses) ->
      List.fold_left (fun s (_, e) -> visit f s e) state clauses


(**
 * Pretty printer
 **)


let fprintf = Format.fprintf

let pp_const ppf = function
  | CNil -> fprintf ppf "nil"
  | CInt n -> fprintf ppf "%d" n
  | CReal f -> fprintf ppf "%f" f
  | CBool b -> fprintf ppf "%B" b
  | CDataCons dc -> fprintf ppf "%s" dc

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
  let pp_list = Pp_utils.pp_list in
  let rec pp ppf e = pp_desc ppf e.expr_desc
  and pp_desc ppf = function
    | EConst c -> fprintf ppf "%a" pp_const c
    | EIdent i -> fprintf ppf "%s" i
    | ETuple es -> fprintf ppf "%a" (pp_list ", " pp) es
    | EFby (v, e) -> fprintf ppf "(%a fby %a)" pp_const v pp e
    | EOp (op, [e1; e2]) -> fprintf ppf "(%a %a %a)" pp e1 pp_op op pp e2
    | EOp (OpNot, [e]) -> fprintf ppf "(%a %a)" pp_op OpNot pp e
    | EOp (OpSub, [e]) -> fprintf ppf "(%a %a)" pp_op OpSub pp e
    | EOp _ -> assert false
    | EApp (f, args, ev) -> fprintf ppf "(%s(%a) every %a)" f (pp_list ", " pp) args pp ev
    | EWhen (e, c, x) -> fprintf ppf "(%a when %s(%s))" pp e c x
    | EMerge (x, clauses) -> fprintf ppf "(merge %s %a)" x (pp_list " " pp_clause) clauses
  and pp_clause ppf (c, e) = fprintf ppf "(%s -> %a)" c pp e
  in pp

let rec pp_pat ppf = function
  | PIdent i -> fprintf ppf "%s" i
  | PTuple pats -> fprintf ppf "(%a)" (Pp_utils.pp_list ", " pp_pat) pats
let pp_pat ppf p = pp_pat ppf p.pat_desc

let pp_ty ppf = function
  | TyBool -> fprintf ppf "bool"
  | TyInt -> fprintf ppf "int"
  | TyReal -> fprintf ppf "real"
  | TyEnum (name, _) -> fprintf ppf "%s" name

let pp_equation ppf eq =
  fprintf ppf "%a = %a" pp_pat eq.eq_pat pp_expr eq.eq_expr

let pp_node ppf n =
  let pp_arg ppf (id, ty) = fprintf ppf "%s: %a" id pp_ty ty in
  let pp_equation ppf eq = fprintf ppf "  %a" pp_equation eq in
  let pp_list = Pp_utils.pp_list in
  fprintf ppf "node %s(%a) = (%a)\nwith var %a in\n%a"
    n.n_name
    (pp_list "; " pp_arg) n.n_input
    (pp_list "; " pp_arg) n.n_output
    (pp_list "; " pp_arg) n.n_local
    (pp_list ";\n" pp_equation) n.n_eqs

let pp_typedef fmt (ty_name, enum) =
  fprintf fmt "type %s = %a" ty_name (Pp_utils.pp_list " + " Format.pp_print_string) enum


let pp_file ppf f =
  let pp_list = Pp_utils.pp_list in
  fprintf ppf "%a" (pp_list "\n" pp_typedef) f.f_typedefs;
  fprintf ppf "\n\n";
  fprintf ppf "%a" (pp_list "\n\n" pp_node) f.f_nodes