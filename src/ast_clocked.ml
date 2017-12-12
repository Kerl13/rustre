open Ast_typed

module Smap = Map.Make(String)

(**
 * Clocked and Typed AST
 **)

type ck =
  | CBase : ck
  | COn : ck * ident * _ var_ident -> ck
  | CVar : cvar -> ck

and cvar = { id: int; mutable def: ck option }

type ct = ck list




(**
 * Expressions
 * tagged with a phantom type corresponding to their lustre type
*)
type 'a cexpr = {
  texpr_desc  : 'a cexpr_desc ;
  texpr_type  : 'a compl_ty ;
  texpr_clock : ct ;
  texpr_loc   : Ast_typed.location
}


and 'a cexpr_desc =
  | CConst : 'a const -> 'a ty cexpr_desc
  | CIdent : 'a ty var_ident -> 'a ty cexpr_desc
  | CFby   : 'a const * 'a ty cexpr -> 'a ty cexpr_desc
  | CBOp   : ('a, 'b) binop * 'a ty cexpr * 'a ty cexpr -> 'b ty cexpr_desc
  | CUOp   : ('a, 'b) unop * 'a ty cexpr -> 'b ty cexpr_desc
  | CApp   : ('a, 'b) tagged_ident * 'a cexpr_list * bool ty cexpr -> 'b cexpr_desc
  | CWhen  : 'a ty cexpr * ident * 'b ty var_ident -> 'a ty cexpr_desc
  | CMerge : ident * (ident * 'a ty cexpr) list -> 'a ty cexpr_desc

and 'a cexpr_list =
  | CLNil : unit cexpr_list
  | CLSing : 'a ty cexpr -> 'a ty cexpr_list
  | CLCons : 'a ty cexpr * 'b cexpr_list -> ('a ty * 'b) cexpr_list


(** Programs *)
type file = {
  cf_typedefs : (ident * ident list) list ;
  cf_nodes : node list
}

and node = Node: ('a, 'b) node_desc -> node

and ('a, 'b) node_desc = {
  n_name   : ('a, 'b) tagged_ident;
  n_input  : 'a var_list;
  n_output : 'b var_list;
  n_local  : node_local;
  n_eqs    : equation list ;
  n_loc    : Ast_typed.location ;
  n_clocks : ck Smap.t
}

and equation = Equ: 'a pattern * 'a cexpr -> equation


(** Pretty printing *)

let symbol_list () =
  let i = ref (-1) in
  fun () ->
    incr i ;
    let letter = match !i mod 4 with
      | 0 -> "α"
      | 1 -> "β"
      | 2 -> "γ"
      | 3 -> "δ"
      | _ -> assert false
    in
    if !i < 4 then letter
    else letter ^ (string_of_int (!i / 4))

let symbol_map () =
  let symbols = Hashtbl.create 17 in
  let next_symbol = symbol_list () in
  function var ->
    if Hashtbl.mem symbols var.id then
      Hashtbl.find symbols var.id
    else begin
      let s = next_symbol () in
      Hashtbl.add symbols var.id s ;
      s
    end

let fprintf = Format.fprintf

let rec pp_ck syms fmt = function
  | CBase -> fprintf fmt "base"
  | COn (ck, dc, x) -> fprintf fmt "%a on %s(%s)" (pp_ck syms) ck dc x
  | CVar v -> fprintf fmt "%s" (syms v)

let rec pp_vl : type a. ck Smap.t -> (cvar -> string) -> Format.formatter -> a var_list -> unit
  = fun env syms fmt vl ->
    let pp_ck = pp_ck syms in
    match vl with
    | VIdent (i, _) -> fprintf fmt "    %s : [%a]" i pp_ck (Smap.find i env)
    | VEmpty -> ()
    | VTuple(i, _, b) -> fprintf fmt "    %s : [%a]\n%a" i pp_ck (Smap.find i env) (pp_vl env syms) b

let pp_clocks_node fmt (Node n : node) =
  let Ast_typed.Tagged(_, _, name) = n.n_name in
  let Ast_typed.NodeLocal n_local = n.n_local in
  let symbol_map = symbol_map () in
  let pp_vl fmt vl = pp_vl n.n_clocks symbol_map fmt vl in
  fprintf fmt "node %s\n" name;
  fprintf fmt "  inputs:\n";
  fprintf fmt "%a" pp_vl n.n_input;
  fprintf fmt "\n  locals:\n";
  fprintf fmt "%a" pp_vl n_local;
  fprintf fmt "\n  outputs:\n";
  fprintf fmt "%a" pp_vl n.n_output

let pp_clocks_file fmt file =
  fprintf fmt "%a" (Pp_utils.pp_list "\n\n" pp_clocks_node) file.cf_nodes
