(**
 * Extended AST with automata, reset and match constructs
 **)

include Common_defs

type expr = {
  expr_desc  : expr_desc ;
  expr_loc   : location
}

and expr_desc =
  | EConst of const
  | EIdent of ident
  | ELast of ident
  | ETuple of expr list
  | EFby   of const * expr
  | EOp    of op * expr list
  | EApp   of ident * expr list * expr
  | EWhen  of expr * ident * ident
  | EMerge of ident * (ident * expr) list

type file = {
  f_typedefs : enum list ;
  f_nodes : node list
}

and node = {
  n_name   : ident ;
  n_input  : (ident * ty) list ;
  n_output : (ident * ty) list ;
  n_local  : var_decl list ;
  n_eqs    : equation list ;
  n_loc    : location
}

and equation = {
  eq_desc : eq_desc ;
  eq_loc : location
}

and var_decl = {
  v_name : ident ;
  v_type : ty ;
  v_shared : bool ;
  v_init : const option;
  v_loc : location
}

and eq_desc =
  | EEq of pattern * expr
  | EMatch of ident * match_handler list
  | EReset of equation list * expr
  | EAutomaton of state_handler list

and pattern = {
  pat_desc : pattern_desc ;
  pat_loc  : location
}

and pattern_desc =
  | PIdent of ident
  | PTuple of pattern_desc list

and match_handler = {
  m_name : ident ;
  m_eqs : equation list;
}

and state_handler = {
  s_name : ident ;
  s_local : var_decl list ;
  s_eqs : equation list ;
  s_until : escape list ;
  s_unless : escape list ;
}

and escape = {
  e_cond : expr ;
  e_reset : bool ;
  e_next : ident
}

(** A visitor for expressions *)
let rec visit_expr f state e =
  let state = f state e.expr_desc in
  match e.expr_desc with
  | EConst _ | EIdent _ | ELast _ -> state
  | ETuple es -> List.fold_left (visit_expr f) state es
  | EFby (_, e) -> visit_expr f state e
  | EOp (_, es) -> List.fold_left (visit_expr f) state es
  | EApp (_, es, every) -> List.fold_left (visit_expr f) (visit_expr f state every) es
  | EWhen (e, _, _) -> visit_expr f state e
  | EMerge (_, clauses) ->
      List.fold_left (fun s (_, e) -> visit_expr f s e) state clauses

and visit_eq f state {eq_desc ; _ } =
  match eq_desc with
  | EEq (_, e) -> visit_expr f state e
  | EMatch (_, ms) -> List.fold_left (visit_match f) state  ms
  | EReset (eqs, e) -> List.fold_left (visit_eq f) (visit_expr f state e) eqs
  | EAutomaton sts -> List.fold_left (visit_state f) state sts

and visit_match f state {m_eqs ; _} =
  List.fold_left (visit_eq f) state m_eqs

and visit_state f state {s_eqs ; s_until ; s_unless ; _} =
  let visit_escs = List.fold_left (fun state esc -> visit_expr f state esc.e_cond) in
  let state = visit_escs state s_until in
  let state = visit_escs state s_unless in
  List.fold_left (visit_eq f) state s_eqs


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
    | ELast i -> fprintf ppf "last %s" i
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

let pp_var_decl ppf {v_name ; v_type ; v_shared ; v_init ; _} =
  fprintf ppf "%s%s: %a%a" (if v_shared then "shared " else "") v_name
    pp_ty v_type (fun ppf i -> match i with None -> fprintf ppf ""
                               | Some e -> fprintf ppf " = %a" pp_const e) v_init

let rec pp_equation ind ppf {eq_desc ; _} =
  let pp_list = Pp_utils.pp_list in
  let pp ppf = function
  | EEq (pat, e) -> fprintf ppf "%a = %a" pp_pat pat pp_expr e
  | EMatch (x, hl) -> fprintf ppf "match %s with\n%a" x (pp_list "\n" (pp_match ind)) hl
  | EReset (el, e) -> fprintf ppf "reset\n%a\n%severy %a" (pp_list ";\n" (pp_equation (ind^"  "))) el ind pp_expr e
  | EAutomaton hl -> fprintf ppf "automaton\n%a\n%send" (pp_list "\n" (pp_state (ind^"  "))) hl ind
  in fprintf ppf "%s%a" ind pp eq_desc

and pp_match ind ppf {m_name ; m_eqs } =
  let pp_list = Pp_utils.pp_list in
  fprintf ppf "%s| %s do\n%a\n%s  done" ind m_name (pp_list ";\n" (pp_equation (ind ^ "    "))) m_eqs ind

and pp_state ind ppf { s_name ; s_local ; s_eqs ; s_until ; s_unless } =
  let pp_list = Pp_utils.pp_list in
  fprintf ppf "%sstate %s\n%svar %a in do\n%a\n%a%a" ind s_name (ind^"  ")
    (pp_list " ; " pp_var_decl) s_local
    (pp_list ";\n" (pp_equation (ind^"  "))) s_eqs
    (pp_list "\n" (pp_escape "until" ind)) s_until
    (pp_list "\n" (pp_escape "unless" ind)) s_unless

and pp_escape s ind ppf {e_cond ; e_reset ; e_next} =
  fprintf ppf "%s%s %a %s %s" ind s pp_expr e_cond (if e_reset then "then" else "continue") e_next


let pp_node ppf n =
  let pp_arg ppf (id, ty) = fprintf ppf "%s: %a" id pp_ty ty in
  let pp_list = Pp_utils.pp_list in
  fprintf ppf "node %s(%a) = (%a)\nwith var %a in\n%a"
    n.n_name
    (pp_list "; " pp_arg) n.n_input
    (pp_list "; " pp_arg) n.n_output
    (pp_list "; " pp_var_decl) n.n_local
    (pp_list ";\n" (pp_equation "  ") ) n.n_eqs

let pp_typedef fmt (ty_name, enum) =
  fprintf fmt "type %s = %a" ty_name (Pp_utils.pp_list " + " Format.pp_print_string) enum


let pp_file ppf f =
  let pp_list = Pp_utils.pp_list in
  fprintf ppf "%a" (pp_list "\n" pp_typedef) f.f_typedefs;
  fprintf ppf "\n\n";
  fprintf ppf "%a" (pp_list "\n\n" pp_node) f.f_nodes
