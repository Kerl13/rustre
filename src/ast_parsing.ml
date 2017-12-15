(**
 * Parsing AST without type and clock annotations
 **)

include Common_defs

exception MError of string


      
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
  | CReal f -> fprintf ppf "%.16g" f
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
  let rec pp need_pars ppf e = pp_desc need_pars ppf e.expr_desc
  and pp_desc need_pars ppf = function
    | EConst c -> fprintf ppf "%a" pp_const c
    | EIdent i -> fprintf ppf "%s" i
    | ETuple es -> fprintf ppf "(%a)" (Pp_utils.pp_list ", " (pp false)) es
    | EFby (v, e) ->
      if need_pars then fprintf ppf "(" ;
      fprintf ppf "@[<2>%a@ fby@ %a@]" pp_const v (pp true) e ;
      if need_pars then fprintf ppf ")"
    | EOp (op, [e1; e2]) ->
      if need_pars then fprintf ppf "(" ;
      fprintf ppf "%a %a %a" (pp true) e1 pp_op op (pp true) e2 ;
      if need_pars then fprintf ppf ")"
    | EOp (OpNot as op, [e])
    | EOp (OpSub as op, [e]) ->
      if need_pars then fprintf ppf "(" ;
      fprintf ppf "%a %a" pp_op op (pp true) e ;
      if need_pars then fprintf ppf ")"
    | EOp _ -> failwith "Ill formed operation. Should be rejected by the parser"
    | EApp (f, args, ev) ->
      if need_pars then fprintf ppf "(" ;
      fprintf ppf "@[%s(%a)@ every %a@]" f (Pp_utils.pp_list ", " (pp false)) args (pp true) ev ;
      if need_pars then fprintf ppf ")"
    | EWhen (e, c, x) ->
      if need_pars then fprintf ppf "(" ;
      fprintf ppf "%a when %s(%s)" (pp true) e c x ;
      if need_pars then fprintf ppf ")"
    | EMerge (x, clauses) ->
      fprintf ppf "@[<2>merge %s@\n%a@]" x (Pp_utils.pp_list_n "" pp_clause) clauses ;
      if need_pars then fprintf ppf ")"
  and pp_clause ppf (c, e) = fprintf ppf "(@[<2>%s ->@ %a@])" c (pp false) e
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
  fprintf ppf "@[<2>%a =@ %a@]" pp_pat eq.eq_pat (pp_expr false) eq.eq_expr

let pp_node ppf n =
  let pp_arg ppf (id, ty) = fprintf ppf "%s: %a" id pp_ty ty in
  let pp_args = Pp_utils.pp_list "; " pp_arg in
  fprintf ppf "@[node %s(%a) = (%a)@\n" n.n_name pp_args n.n_input pp_args n.n_output ;
  if n.n_local = []
  then fprintf ppf "@[<2>with@\n"
  else fprintf ppf "@[<2>with var %a in@\n" pp_args n.n_local ;
  fprintf ppf "%a@]@]@\n@\n" (Pp_utils.pp_list_n " ;" pp_equation) n.n_eqs

let pp_typedef fmt (ty_name, enum) =
  fprintf fmt "type %s = %a" ty_name (Pp_utils.pp_list " + " Format.pp_print_string) enum

let pp_file ppf f =
  fprintf ppf "%a@\n@\n" (Pp_utils.pp_list_n "\n" pp_typedef) f.f_typedefs;
  fprintf ppf "%a" (Pp_utils.pp_list "" pp_node) f.f_nodes
