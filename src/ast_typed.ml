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
  | EPair  : 'a expr * 'b expr -> ('a * 'b) expr_desc
  | EFby   : 'a const * 'a expr -> 'a expr_desc
  | EBOp    : ('a, 'b) binop * 'a expr * 'a expr -> 'b expr_desc
  | EUOp    : ('a, 'b) unop * 'a expr -> 'b expr_desc
  | App    : ('a, 'b) tagged_ident * 'a expr * 'c expr -> 'b expr_desc
  | EWhen  : 'a expr * ident * 'b var_ident -> 'a expr_desc
  | EMerge : ident * (ident * 'a expr) list -> 'a expr_desc

and _ const =
  | CNil  : unit -> 'a const
  | CBool : bool -> bool const
  | CInt  : int -> int num_ty const
  | CReal : float -> float num_ty const



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
