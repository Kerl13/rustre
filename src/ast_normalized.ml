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
  | EquApp: 'b pattern * ('a, 'b) tagged_ident * 'a var_list * bool nexpr * Ast_clocked.ct -> nequation



let fprintf = Format.fprintf
let pp_list = Pp_utils.pp_list

let rec pp_expr : type a. Format.formatter -> a nexpr -> unit
  = fun fmt e -> match e.nexpr_desc with
  | NConst c -> fprintf fmt "%a" pp_const c
  | NIdent x -> fprintf fmt "%s" x
  | NBOp (bop, e1, e2) -> fprintf fmt "(%a %a %a)" pp_expr e1 pp_bop bop pp_expr e2
  | NUOp (uop , e) -> fprintf fmt "(%a %a)" pp_uop uop pp_expr e
  | NWhen (e, dc, x) -> fprintf fmt "(%a when %s(%s))" pp_expr e dc x

let rec pp_expr_merge : type a. Format.formatter -> a nexpr_merge -> unit
  = fun fmt e -> match e.nexpr_merge_desc with
  | NMerge (x, clauses) -> fprintf fmt "merge %s %a" x (pp_list " " pp_clause) clauses
  | NExpr e -> pp_expr fmt e
and pp_clause: type a. Format.formatter -> ident * a nexpr_merge -> unit
  = fun fmt (c, e) -> fprintf fmt "(%s -> %a)" c pp_expr_merge e

let rec pp_vl: type a. 'b -> a var_list -> unit = fun ppf -> function
  | VIdent (i, ty) -> fprintf ppf "%s:%a" i pp_ty ty
  | VEmpty -> fprintf ppf "()"
  | VTuple(i, ty, b) -> fprintf ppf "%s:%a, (%a)" i pp_ty ty pp_vl b

let pp_eq fmt = function
  | EquSimple (x, e) -> fprintf fmt "%s = %a" x pp_expr_merge e
  | EquFby (x, c, e) -> fprintf fmt "%s = %a fby %a" x pp_const c pp_expr e
  | EquApp (pat, f, args, every, _) ->
      let Ast_typed.Tagged(_, _, f) = f in
      fprintf fmt "(%a) = (%s(%a) every %a)" pp_vl pat.pat_desc f pp_vl args pp_expr every

let pp_node fmt (NNode n) =
  let pp_eq fmt eq = fprintf fmt "  %a" pp_eq eq in
  let Tagged(_, _, name) = n.n_name in
  let NodeLocal n_local = n.n_local in
  fprintf fmt "node %s(%a) = (%a)\nwith var %a in\n%a"
    name
    pp_vl n.n_input
    pp_vl n.n_output
    pp_vl n_local
    (pp_list ";\n" pp_eq) n.n_eqs

let pp_file fmt f =
  fprintf fmt "%a\n\n" (pp_list "\n" Ast_parsing.pp_typedef) f.nf_typedefs ;
  fprintf fmt "%a" (pp_list "\n\n" pp_node) f.nf_nodes
