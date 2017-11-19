type ident = Ast_parsing.ident
type location = Ast_parsing.location


(**
 * Typed AST, no clock annotation
 **)


type 'a num_ty =
  | TyZ : int num_ty
  | TyReal : float num_ty

(**
 * Types
 * tagged with a phantom type
*)
type _ ty =
  | TyBool : bool ty
  | TyNum  : 'a num_ty -> 'a num_ty ty
  | TyPair : 'a ty * 'b ty -> ('a * 'b) ty


type _ var_ident = Ast_parsing.ident

type _ var_list =
  | VIdent: 'a var_ident * 'a ty -> 'a var_list
  | VEmpty: unit var_list
  | VTuple: 'a var_ident * 'a ty * 'b var_list -> ('a * 'b) var_list

type (_, _) tagged_ident = Tagged: ('a var_list) * ('b var_list) * ident -> ('a, 'b) tagged_ident

(**
 * Binary operators:
 * (type of its arguments, type of its result) *)
type (_, _) binop =
  | OpAdd  : ('a num_ty, 'a num_ty) binop
  | OpSub  : ('a num_ty, 'a num_ty) binop
  | OpMul  : ('a num_ty, 'a num_ty) binop
  | OpDiv  : ('a num_ty, 'a num_ty) binop
  | OpMod  : ('a num_ty, 'a num_ty) binop
  | OpLt   : ('a num_ty, bool) binop
  | OpLe   : ('a num_ty, bool) binop
  | OpGt   : ('a num_ty, bool) binop
  | OpGe   : ('a num_ty, bool) binop
  | OpEq   : ('a, bool) binop
  | OpNeq   : ('a, bool) binop
  | OpAnd  : (bool, bool) binop
  | OpOr   : (bool, bool) binop
  | OpImpl : (bool, bool) binop

(**
 * Builtin unary operators
 * tagged with two phantom types :
 * - the type of their arguments
 * - the type of their result
*)
type (_, _) unop =
  | OpNot  : (bool, bool) unop
  | OpUMinus : ('a num_ty, 'a num_ty) unop



type _ const =
  | CNil  : 'a const
  | CBool : bool -> bool const
  | CInt  : int -> int num_ty const
  | CReal : float -> float num_ty const

(**
 * Expressions
 * tagged with a phantom type corresponding to their lustre type
*)
type 'a expr = {
  texpr_desc : 'a expr_desc ;
  texpr_type : 'a ty ;
  texpr_loc  : location
}

and 'a expr_desc =
  | EConst : 'a const -> 'a expr_desc
  | EIdent : 'a var_ident -> 'a expr_desc
  | EFby   : 'a const * 'a expr -> 'a expr_desc
  | EBOp    : ('a, 'b) binop * 'a expr * 'a expr -> 'b expr_desc
  | EUOp    : ('a, 'b) unop * 'a expr -> 'b expr_desc
  | EApp    : ('a, 'b) tagged_ident * 'a expr_list * bool expr -> 'b expr_desc
  | EWhen  : 'a expr * ident * 'b var_ident -> 'a expr_desc
  | EMerge : ident * (ident * 'a expr) list -> 'a expr_desc

and 'a expr_list =
  | ELNil : unit expr_list
  | ELSing : 'a expr -> 'a expr_list
  | ELCons : 'a expr * 'b expr_list -> ('a * 'b) expr_list



(** Programs *)
type file = node list

and node = Node: ('a, 'b) node_desc -> node

and ('a, 'b) node_desc = {
  n_name   : ('a, 'b) tagged_ident;
  n_input  : 'a var_list;
  n_output : 'b var_list;
  n_local  : node_local;
  n_eqs    : equation list ;
  n_loc    : location
}

and node_local = NodeLocal: 'c var_list -> node_local

and equation = Equ: 'a pattern * 'a expr -> equation

and 'a pattern = {
  pat_desc : 'a pattern_desc ;
  pat_loc  : location
}

and 'a pattern_desc = 'a var_list






(**
 * Pretty printer
 **)

let fprintf = Format.fprintf

let rec pp_list sep pp ppf = function
  | [] -> fprintf ppf ""
  | [x] -> fprintf ppf "%a" pp x
  | x :: xs -> fprintf ppf "%a%s%a" pp x sep (pp_list sep pp) xs

let pp_const: type a. 'b -> a const -> unit = fun ppf -> function
  | CNil -> fprintf ppf "nil"
  | CInt n -> fprintf ppf "%d" n
  | CReal f -> fprintf ppf "%f" f
  | CBool b -> fprintf ppf "%B" b

let pp_bop: type a b. 'c -> (a, b) binop -> unit = fun ppf -> function
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

let pp_uop: type a b. 'c -> (a, b) unop -> unit = fun ppf -> function
  | OpNot -> fprintf ppf "not"
  | OpUMinus -> fprintf ppf "-"



let rec pp_ty: type a. 'b -> a ty -> unit = fun ppf -> function
  | TyBool -> fprintf ppf "bool"
  | TyNum TyZ -> fprintf ppf "int"
  | TyNum TyReal -> fprintf ppf "real"
  | TyPair(a, b) -> fprintf ppf "%a, %a" pp_ty a pp_ty b

let pp_expr: type a. 'c -> a expr -> unit =
  let rec pp: type a. 'd -> a expr -> unit = fun ppf e -> fprintf ppf "%a:%a" pp_desc e.texpr_desc pp_ty e.texpr_type
  and pp_elist: type a. 'd -> a expr_list -> unit = fun ppf l ->
    match l with
    | ELNil -> ()
    | ELSing a -> pp ppf a
    | ELCons (a,b) -> fprintf ppf "%a, %a" pp a pp_elist b
  and pp_desc: type b. 'c -> b expr_desc -> unit = fun ppf -> function
    | EConst c -> fprintf ppf "%a" pp_const c
    | EIdent i -> fprintf ppf "%s" i
    | EFby (v, e) -> fprintf ppf "(%a fby %a)" pp_const v pp e
    | EBOp (op, e1, e2) -> fprintf ppf "(%a %a %a)" pp e1 pp_bop op pp e2
    | EUOp (op, e) -> fprintf ppf "(%a %a)" pp_uop op pp e
    | EApp (f, args, ev) ->
      let Tagged(_, _, f) = f in
      fprintf ppf "(%s(%a) every %a)" f pp_elist args pp ev
    | EWhen (e, c, x) -> fprintf ppf "(%a when %s(%s))" pp e c x
    | EMerge (x, clauses) -> fprintf ppf "(merge %s %a)" x (pp_list " " pp_clause) clauses
  and pp_clause: type a. 'd -> ident * a expr -> unit = fun ppf (c, e) -> fprintf ppf "(%s -> %a)" c pp e
  in pp

let rec pp_vl: type a. 'b -> a var_list -> unit = fun ppf -> function
  | VIdent (i, ty) -> fprintf ppf "%s:%a" i pp_ty ty
  | VEmpty -> fprintf ppf "()"
  | VTuple(i, ty, b) -> fprintf ppf "%s:%a, (%a)" i pp_ty ty pp_vl b

let pp_pat ppf p = pp_vl ppf p.pat_desc

let pp_equation ppf (Equ(pat, expr)) =
  fprintf ppf "%a = %a" pp_pat pat pp_expr expr

let pp_node ppf (Node n) =
  let pp_equation ppf eq = fprintf ppf "  %a" pp_equation eq in
  let Tagged(_, _, name) = n.n_name in
  let NodeLocal n_local = n.n_local in
  fprintf ppf "node %s(%a) = (%a)\nwith var %a in\n%a"
    name
    pp_vl n.n_input
    pp_vl n.n_output
    pp_vl n_local
    (pp_list ";\n" pp_equation) n.n_eqs

let pp_file ppf f = fprintf ppf "%a" (pp_list "\n\n" pp_node) f
