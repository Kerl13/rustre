type ident = string
type location = Lexing.position * Lexing.position


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
