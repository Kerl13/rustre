open Ast_typed
open Ast_clocked

(**
 * Expressions
 * tagged with a phantom type corresponding to their lustre type
 * named `a` in the article
*)
type 'a nexpr = {
  nexpr_desc  : 'a nexpr_desc ;
  nexpr_type  : 'a ty ;
  nexpr_clock : ct ;
  nexpr_loc   : location
}

(** named `e` in the article *)
and 'a nexpr_desc =
  | CConst : 'a const -> 'a nexpr_desc
  | CIdent : 'a var_ident -> 'a nexpr_desc
  | CBOp    : ('a, 'b) binop * 'a nexpr * 'a nexpr -> 'b nexpr_desc
  | CUOp    : ('a, 'b) unop * 'a nexpr -> 'b nexpr_desc
  | CWhen  : 'a nexpr * ident * 'b var_ident -> 'a nexpr_desc
  | CPair  : 'a nexpr * 'b nexpr -> ('a * 'b) nexpr_desc

(** named `ce` in the article *)
type _ nexpr_merge_desc =
| CMerge : ident * (ident * 'a nexpr_merge) list -> 'a nexpr_merge_desc

(** named `ca` in the article *)
and 'a nexpr_merge = {
  nexpr_merge_desc: 'a nexpr_merge_desc;
  nexpr_merge_type: 'a ty;
  nexpr_merge_clock: ct;
  nexpr_merge_loc: location;
}


(** Programs *)
type nfile = nnode list

and nnode = Node: ('a, 'b) nnode_desc -> nnode

and ('a, 'b) nnode_desc = {
  n_name   : ('a, 'b) tagged_ident;
  n_input  : 'a var_list;
  n_output : 'b var_list;
  n_local  : node_local;
  n_eqs    : nequation list ;
  n_loc    : location
}

and nequation =
| EquSimple: 'a pattern * 'a nexpr_merge -> nequation
| EquFby: 'a pattern * 'a const * 'a nexpr -> nequation
| EquApp: 'b pattern * ('a, 'b) tagged_ident * 'a nexpr * 'c nexpr -> nequation
