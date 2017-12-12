(* Definitions used by all ASTs *)

type ident = string
type location = Lexing.position * Lexing.position


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
