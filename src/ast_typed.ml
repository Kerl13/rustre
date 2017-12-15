type ident = Ast_parsing.ident
type location = Ast_parsing.location


(**
 * Typed AST, no clock annotation
 **)


type 'a num_ty =
  | TyZ : int num_ty
  | TyReal : float num_ty

type enum

(**
 * Types
 * tagged with a phantom type
*)
type _ ty =
  | TyBool : bool ty
  | TyNum  : 'a num_ty -> 'a num_ty ty
  | TyEnum : string * string list -> enum ty

type _ compl_ty =
  | TySing : 'a ty -> 'a ty compl_ty
  | TyNil  : unit compl_ty
  | TyPair : 'a ty * 'b compl_ty -> ('a ty * 'b) compl_ty


type _ var_ident = Ast_parsing.ident

type _ var_list =
  | VIdent: 'a var_ident * 'a ty -> 'a ty var_list
  | VEmpty: unit var_list
  | VTuple: 'a var_ident * 'a ty * 'b var_list -> ('a ty * 'b) var_list

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
  | OpNeq  : ('a, bool) binop
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
  | CDataCons : ident -> enum const

(**
 * Expressions
 * tagged with a phantom type corresponding to their lustre type
*)
type 'a expr = {
  texpr_desc : 'a expr_desc ;
  texpr_type : 'a compl_ty ;
  texpr_loc  : location
}

and 'a expr_desc =
  | EConst : 'a const -> 'a ty expr_desc
  | EIdent : 'a ty var_ident -> 'a ty expr_desc
  | EFby   : 'a const * 'a ty expr -> 'a ty expr_desc
  | EBOp   : ('a, 'b) binop * 'a ty expr * 'a ty expr -> 'b ty expr_desc
  | EUOp   : ('a, 'b) unop * 'a ty expr -> 'b ty expr_desc
  | EApp   : ('a, 'b) tagged_ident * 'a expr_list * bool ty expr -> 'b expr_desc
  | EWhen  : 'a ty expr * ident * 'b ty var_ident -> 'a ty expr_desc
  | EMerge : ident * (ident * 'a ty expr) list -> 'a ty expr_desc

and 'a expr_list =
  | ELNil : unit expr_list
  | ELSing : 'a ty expr -> 'a ty expr_list
  | ELCons : 'a ty expr * 'b expr_list -> ('a ty * 'b) expr_list

(** Programs *)
type 'a pattern = {
  pat_desc : 'a pattern_desc ;
  pat_loc  : location
}

and 'a pattern_desc = 'a var_list


type file = {
  tf_typedefs : (ident * ident list) list ;
  tf_nodes : node list
}

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







(**
 * Pretty printer
 **)

let fprintf = Format.fprintf

let pp_const: type a. 'b -> a const -> unit = fun ppf -> function
  | CNil -> fprintf ppf "nil"
  | CInt n -> fprintf ppf "%d" n
  | CReal f -> fprintf ppf "%.16g" f
  | CBool b -> fprintf ppf "%B" b
  | CDataCons dc -> fprintf ppf "%s" dc

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



let pp_ty: type a. 'b -> a ty -> unit = fun ppf -> function
  | TyBool -> fprintf ppf "bool"
  | TyNum TyZ -> fprintf ppf "int"
  | TyNum TyReal -> fprintf ppf "real"
  | TyEnum (name, _) -> fprintf ppf "%s" name

let rec pp_compl_ty: type a. 'b -> a compl_ty -> unit = fun ppf l ->
  match l with
  | TySing a -> pp_ty ppf a
  | TyNil -> fprintf ppf "nil"
  | TyPair (a,b) ->
    fprintf ppf "%a, %a" pp_ty a pp_compl_ty b

let pp_expr: type a. bool -> 'c -> a expr -> unit =
  let rec pp: type a.  bool -> 'd -> a expr -> unit
    = fun need_pars ppf e ->
      fprintf ppf "%a:%a" (pp_desc need_pars) e.texpr_desc pp_compl_ty e.texpr_type

  and pp_elist: type a. 'd -> a expr_list -> unit = fun ppf l ->
    match l with
    | ELNil -> ()
    | ELSing a -> pp false ppf a
    | ELCons (a,b) -> fprintf ppf "%a, %a" (pp false) a pp_elist b

  and pp_desc: type b. bool -> 'c -> b expr_desc -> unit
    = fun need_pars ppf -> function
    | EConst c -> fprintf ppf "%a" pp_const c
    | EIdent i -> fprintf ppf "%s" i
    | EFby (v, e) ->
      if need_pars then fprintf ppf "(" ;
      fprintf ppf "@[<2>%a@ fby@ %a@]" pp_const v (pp true) e ;
      if need_pars then fprintf ppf ")"
    | EBOp (op, e1, e2) ->
      if need_pars then fprintf ppf "(" ;
      fprintf ppf "%a %a %a" (pp true) e1 pp_bop op (pp true) e2 ;
      if need_pars then fprintf ppf ")"
    | EUOp (op, e) ->
      if need_pars then fprintf ppf "(" ;
      fprintf ppf "%a %a" pp_uop op (pp true) e ;
      if need_pars then fprintf ppf ")"
    | EApp (f, args, ev) ->
      let Tagged(_, _, f) = f in
      if need_pars then fprintf ppf "(" ;
      fprintf ppf "@[%s(%a)@ every %a@]" f pp_elist args (pp true) ev ;
      if need_pars then fprintf ppf ")"
    | EWhen (e, c, x) ->
      if need_pars then fprintf ppf "(" ;
      fprintf ppf "%a when %s(%s)" (pp true) e c x ;
      if need_pars then fprintf ppf ")"
    | EMerge (x, clauses) -> fprintf ppf "(@[<2>merge %s@\n%a@])" x (Pp_utils.pp_list_n "" pp_clause) clauses

  and pp_clause: type a. 'd -> ident * a expr -> unit
    = fun ppf (c, e) -> fprintf ppf "(@[<2>%s ->@ %a@])" c (pp false) e
  in pp

let rec pp_vl: type a. 'b -> a var_list -> unit = fun ppf -> function
  | VIdent (i, ty) -> fprintf ppf "%s:%a" i pp_ty ty
  | VEmpty -> fprintf ppf "()"
  | VTuple(i, ty, b) -> fprintf ppf "%s:%a, (%a)" i pp_ty ty pp_vl b

let pp_pat ppf p = pp_vl ppf p.pat_desc

let pp_equation ppf (Equ(pat, expr)) =
  fprintf ppf "@[<2>%a =@ %a@]" pp_pat pat (pp_expr false) expr

let var_list_empty : type a. a var_list -> bool = function
  | VEmpty -> true
  | _ -> false

let pp_node ppf (Node n) =
  let Tagged(_, _, name) = n.n_name in
  let NodeLocal n_local = n.n_local in
  fprintf ppf "@[node %s(%a) = (%a)@\n" name pp_vl n.n_input pp_vl n.n_output ;
  if var_list_empty n_local
  then fprintf ppf "@[<2>with@\n"
  else fprintf ppf "@[<2>with var %a in@\n" pp_vl n_local ;
  fprintf ppf "%a@]@]@\n@\n" (Pp_utils.pp_list_n " ;" pp_equation) n.n_eqs


let pp_file fmt f =
  fprintf fmt "%a@\n@\n" (Pp_utils.pp_list_n "\n" Ast_parsing.pp_typedef) f.tf_typedefs ;
  fprintf fmt "%a" (Pp_utils.pp_list "" pp_node) f.tf_nodes
