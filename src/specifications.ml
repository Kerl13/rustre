open Pp_utils
open Ast_parsing

let fprintf = Format.fprintf

let spec_const ppf c =
  match c with
  | CNil -> assert false
  | CInt i -> fprintf ppf "%d" i
  | CReal f -> fprintf ppf "%f" f
  | CBool b -> fprintf ppf "%b" b
  | CDataCons i -> fprintf ppf "%s" i

let spec_op ppf o =
  match o with
  | OpAdd -> fprintf ppf "splus"
  | OpSub -> fprintf ppf "ssub"
  | OpAnd -> fprintf ppf "sand"
  | OpOr -> fprintf ppf "sor"
  | OpEq -> fprintf ppf "seq"
  | _ -> assert false

let rec spec_expr ppf expr =
  match expr.expr_desc with
  | Ast_parsing.EConst c -> fprintf ppf "(sconst %a)" spec_const c
  | Ast_parsing.EIdent s -> fprintf ppf "%s" s
  | Ast_parsing.ETuple l -> fprintf ppf "(%a)" (pp_list ", " spec_expr) l
  | Ast_parsing.EFby (c, e) -> fprintf ppf "sfby %a %a" spec_const c spec_expr e
  | Ast_parsing.EOp (op, l) -> fprintf ppf "(%a %a)" spec_op op (pp_list " " spec_expr) l
  | Ast_parsing.EApp (a,b,_) -> assert false
  | Ast_parsing.EWhen (_,_,_) -> assert false
  | Ast_parsing.EMerge (_,_) -> assert false

let rec spec_pat_desc ppf pat =
  match pat with
  | PIdent pi ->
    fprintf ppf "%s" pi
  | PTuple l ->
    fprintf ppf "(%a)" (pp_list ", " spec_pat_desc) l

let rec spec_pat_desc_flat ppf pat =
  match pat with
  | PIdent pi ->
    fprintf ppf "%s" pi
  | PTuple l ->
    fprintf ppf "%a" (pp_list " " spec_pat_desc) l

let spec_eq ppf eq =
  match eq.eq_expr.expr_desc with
  | EApp(a, b, _) ->
    fprintf ppf "Spec%s.spec %a %a" a
      (pp_list " " spec_expr) b
      spec_pat_desc_flat eq.eq_pat.pat_desc
  | _ ->
    fprintf ppf "%a = %a" spec_pat_desc eq.eq_pat.pat_desc spec_expr eq.eq_expr

let find_calls eqs =
  let rec find_call_expr e = match e.expr_desc with
    | EConst _ -> []
    | EIdent _ -> []
    | ETuple l -> List.map find_call_expr l |> List.concat
    | EFby(_, e) -> find_call_expr e
    | EOp(_, e1) -> List.map find_call_expr e1 |> List.concat
    | EApp(s, e1, e2) -> s :: find_call_expr e2 @ (List.map find_call_expr e1 |> List.concat)
    | EWhen (e, _, _) -> find_call_expr e
    | EMerge (_, l) -> List.map snd l |> List.map find_call_expr |> List.concat
  in
  List.map (fun { eq_expr; _ } ->
      find_call_expr eq_expr
    ) eqs
  |> List.concat
  |> List.sort_uniq compare

let spec_node ppf node =
  let var_pp   = (pp_list_brk " " (fun ppf (i, ty) ->
      fprintf ppf "(%s:stream %a)" i pp_ty ty))
  in
  let var_exists = (pp_list_brk " " (fun ppf (i, ty) ->
      fprintf ppf "exists %s: stream %a." i pp_ty ty))
  in
  let pp_import ppf node =
    fprintf ppf "use import int.Int@\nuse import stream.Stream@\n%a"
      (pp_list_n "" (fun ppf l ->
           fprintf ppf "use import Spec%s" l)) (find_calls node.n_eqs)
  in
  fprintf ppf "@[<2>module Spec%s@\n%a@\n@\n@[<2>predicate spec %a %a =@\n%a@\n%a@]@]@\nend"
    node.n_name
    pp_import node
    var_pp node.n_input
    var_pp node.n_output
    var_exists node.n_local
    (pp_list_n "   /\\" spec_eq) node.n_eqs

let spec_file ppf f =
  fprintf ppf "%a" (pp_list_n "\n" spec_node) f.f_nodes
