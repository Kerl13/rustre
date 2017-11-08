open Ast_typed
   
type ident = Ast_parsing.ident
type location = Ast_parsing.location


(**
 * Clocked and Typed AST
 **)

type ck =
  | CBase : ck
  | CCk : ident * 'b var_ident -> ck

type ct =
  | CSingle : ck -> ct
  | CProd : ct list -> ct
  
(**
 * Expressions
 * tagged with a phantom type corresponding to their lustre type
*)
type 'a cexpr = {
  texpr_desc  : 'a cexpr_desc ;
  texpr_type  : 'a ty ;
  texpr_clock : ct ;
  texpr_loc   : location
}

             
and 'a cexpr_desc =
  | CConst : 'a const -> 'a cexpr_desc
  | CIdent : 'a var_ident -> 'a cexpr_desc
  | CPair  : 'a cexpr * 'b cexpr -> ('a * 'b) cexpr_desc
  | CFby   : 'a const * 'a cexpr -> 'a cexpr_desc
  | CBOp    : ('a, 'b) binop * 'a cexpr * 'a cexpr -> 'b cexpr_desc
  | CUOp    : ('a, 'b) unop * 'a cexpr -> 'b cexpr_desc
  | CApp    : ('a, 'b) tagged_ident * 'a cexpr * 'c cexpr -> 'b cexpr_desc
  | CWhen  : 'a cexpr * ident * 'b var_ident -> 'a cexpr_desc
  | CMerge : ident * (ident * 'a cexpr) list -> 'a cexpr_desc


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

and equation = Equ: 'a pattern * 'a cexpr -> equation

and 'a pattern = {
  pat_desc : 'a pattern_desc ;
  pat_loc  : location
}

and 'a pattern_desc = 'a var_list
