open Ast_typed


(**
 * Clocked and Typed AST
 **)

type ck =
  | CBase : ck
  | COn : ck * ident * _ var_ident -> ck
  | CVar : cvar -> ck
  
and ct = ck list
  
and cvar = { id: int; mutable def: ck option}
           

(**
 * Expressions
 * tagged with a phantom type corresponding to their lustre type
*)
type 'a cexpr = {
  texpr_desc  : 'a cexpr_desc ;
  texpr_type  : 'a compl_ty ;
  texpr_clock : ct ;
  texpr_loc   : location
}


and 'a cexpr_desc =
  | CConst : 'a const -> 'a ty cexpr_desc
  | CIdent : 'a ty var_ident -> 'a ty cexpr_desc
  | CFby   : 'a const * 'a ty cexpr -> 'a ty cexpr_desc
  | CBOp    : ('a, 'b) binop * 'a ty cexpr * 'a ty cexpr -> 'b ty cexpr_desc
  | CUOp    : ('a, 'b) unop * 'a ty cexpr -> 'b ty cexpr_desc
  | CApp    : ('a, 'b) tagged_ident * 'a cexpr_list * bool ty cexpr -> 'b cexpr_desc
  | CWhen  : 'a ty cexpr * ident * 'b ty var_ident -> 'a ty cexpr_desc
  | CMerge : ident * (ident * 'a ty cexpr) list -> 'a ty cexpr_desc

and 'a cexpr_list =
  | CLNil : unit cexpr_list
  | CLSing : 'a ty cexpr -> 'a ty cexpr_list
  | CLCons : 'a ty cexpr * 'b cexpr_list -> ('a ty * 'b) cexpr_list


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

and equation = Equ: 'a pattern * 'a cexpr -> equation
