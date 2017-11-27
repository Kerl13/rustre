open Ast_typed
open Ast_clocked

(**
 * Types
 * tagged with a phantom type
*)
type _ sty =
  | StyBool : bool sty
  | StyNum  : 'a num_ty -> 'a num_ty sty

(**
 * Expressions
 * tagged with a phantom type corresponding to their lustre type
 * named `a` in the article
*)
type 'a nexpr = {
  nexpr_desc  : 'a nexpr_desc ;
  nexpr_type  : 'a ty;
  nexpr_clock : ct ;
  nexpr_loc   : location
}

(** named `e` in the article *)
and 'a nexpr_desc =
  | NConst : 'a const -> 'a nexpr_desc
  | NIdent : 'a ty var_ident -> 'a nexpr_desc
  | NBOp    : ('a, 'b) binop * 'a nexpr * 'a nexpr -> 'b nexpr_desc
  | NUOp    : ('a, 'b) unop * 'a nexpr -> 'b nexpr_desc
  | NWhen  : 'a nexpr * ident * 'b ty var_ident -> 'a nexpr_desc
  (** not in the article, paired expressions, i.e. lists *)
and 'a nexpr_paired =
  | NExpr : 'a nexpr -> 'a nexpr_paired
  | Nnil  : unit nexpr_paired
  | NPair : 'a nexpr * 'b nexpr_paired -> ('a * 'b) nexpr_paired

(** named `ce` in the article *)
type _ nexpr_merge_desc =
  | NMerge : ident * (ident * 'a nexpr_merge) list -> 'a nexpr_merge_desc
  | NExpr : 'a nexpr -> 'a nexpr_merge_desc

(** named `ca` in the article *)
and 'a nexpr_merge = {
  nexpr_merge_desc: 'a nexpr_merge_desc;
  nexpr_merge_type: 'a ty;
  nexpr_merge_clock: ct;
  nexpr_merge_loc: location;
}


(** Programs *)
type nfile = {
  nf_typedefs : (ident * ident list) list ;
  nf_nodes : nnode list
}

and nnode = NNode: ('a, 'b) nnode_desc -> nnode

and ('a, 'b) nnode_desc = {
  n_name   : ('a, 'b) tagged_ident;
  n_input  : 'a var_list;
  n_output : 'b var_list;
  n_local  : node_local;
  n_eqs    : nequation list ;
  n_loc    : location
}

and nequation =
  | EquSimple: 'a var_ident * 'a nexpr_merge -> nequation
  | EquFby: 'a var_ident * 'a const * 'a nexpr -> nequation (* the var_ident *must* be local *)
  | EquApp: 'b pattern * ('a, 'b) tagged_ident * 'a var_list * 'c nexpr -> nequation
