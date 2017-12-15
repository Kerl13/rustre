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
  | OpNot -> fprintf ppf "snot"
  | OpSub -> fprintf ppf "ssub"
  | OpAnd -> fprintf ppf "sand"
  | OpOr -> fprintf ppf "sor"
  | OpEq -> fprintf ppf "seq"
  | OpGt -> fprintf ppf "sgt"
  | _ -> assert false

let rec spec_expr ppf expr =
  match expr.expr_desc with
  | Ast_parsing.EConst c -> fprintf ppf "(sconst %a)" spec_const c
  | Ast_parsing.EIdent s -> fprintf ppf "%s" s
  | Ast_parsing.ETuple l -> fprintf ppf "(%a)" (pp_list ", " spec_expr) l
  | Ast_parsing.EFby (c, e) -> fprintf ppf "(sfby %a %a)" spec_const c spec_expr e
  | Ast_parsing.EOp (op, l) -> fprintf ppf "(%a %a)" spec_op op (pp_list " " spec_expr) l
  | Ast_parsing.EApp (_,_,_) -> assert false
  | Ast_parsing.EWhen (a,_,c) -> fprintf ppf "(swhen %s %a)" c spec_expr a
  | Ast_parsing.EMerge (i,l) ->
    let a, b = match l with
      | ["True", a; "False", b] -> a, b
      | ["False", b; "True", a] -> a, b
      | _ -> Format.eprintf "Non Boolean merges are not supported in the semantic proof@\n"; assert false
    in
    fprintf ppf "(smerge %s %a %a)" i spec_expr a spec_expr b

let rec spec_pat_desc ppf pat =
  match pat with
  | PIdent pi ->
    fprintf ppf "%s" pi
  | PTuple l ->
    fprintf ppf "(%a)" (pp_list ", " spec_pat_desc) l

let spec_pat_desc_flat ppf pat =
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

let var_pp_par = (pp_list_brk " " (fun ppf (i, ty) ->
    fprintf ppf "(%s:stream %a)" i pp_ty ty))
let var_pp = (pp_list_brk ", " (fun ppf (i, ty) ->
    fprintf ppf "%s:stream %a" i pp_ty ty))

let var_pp_flat sep f = (pp_list_brk sep (fun ppf (i, _) ->
    fprintf ppf "%a" f i))

let rec spec_print_state pref f states ppf s =
  let st = List.assoc s states in
  if st = [] then
    fprintf ppf "()"
  else
    fprintf ppf "@[<2>{ %a }@]" (pp_list_brk "" (fun ppf sc ->
        match sc with
        | Ast_object.State_var(i, _) -> fprintf ppf "Node%s.%s = %s;" s i (f (pref ^ i))
        | Ast_object.Mach_var (i, target) -> fprintf ppf "%s = %a;" i (spec_print_state (pref ^ i) f states) target)) st

let rec spec_print_state_flat pref states ppf s =
  let st = List.assoc s states in
  let st = List.filter (function
      | Ast_object.Mach_var (_, target) -> List.assoc target states <> []
      | _ -> true) st in
  fprintf ppf "%a" (pp_list_brk ", " (fun ppf sc ->
      match sc with
      | Ast_object.State_var(i, ty) -> fprintf ppf "%s%s: stream %a" pref i pp_ty ty
      | Ast_object.Mach_var (i, target) -> fprintf ppf "%a" (spec_print_state_flat (pref ^ i) states) target)) st


let spec_proof_node ppf (states, node) =
  if node.n_local <> [] then (Format.eprintf "could not prove semantics when there are local variables@."; assert false);
  fprintf ppf "@[<2>lemma valid:@\nforall (* in and out vars *) %a%a (* state *) %a.  @\n"
    var_pp (node.n_input @ node.n_local @ node.n_output)
    (fun ppf () ->
       if List.assoc node.n_name states <> [] then fprintf ppf ", ") ()
    (spec_print_state_flat "s" states) node.n_name;
  fprintf ppf "(* definition by recurrence *)@\n(%a = reset_state /\\@\n"
    (spec_print_state "s" (fun s -> "get " ^ s ^ " O") states) node.n_name;
  fprintf ppf "@[<2>forall n: nat.@ @[<2>step_fonct %a %a %a@])@]@\n"
    (var_pp_flat " " (fun ppf s -> fprintf ppf "(get %s n)" s)) (node.n_input @ node.n_output)
    (spec_print_state "s" (fun s -> "get " ^ s ^ " n") states) node.n_name
    (spec_print_state "s" (fun s -> "get " ^ s ^ " (S n)") states) node.n_name;
  fprintf ppf "(* correction *)@\n-> spec %a %a@]"
    (var_pp_flat " " (fun ppf -> fprintf ppf "%s")) node.n_input
    (var_pp_flat " " (fun ppf -> fprintf ppf "%s")) node.n_output

let spec_node states ppf node =
  let var_exists = (pp_list_brk " " (fun ppf (i, ty) ->
      fprintf ppf "exists %s: stream %a." i pp_ty ty))
  in
  let pp_import ppf node =
    fprintf ppf "use import int.Int@\nuse import stream.Stream@\nuse import test.Node%s@\n%a"
      node.n_name
      (pp_list_n "" (fun ppf l ->
           fprintf ppf "use Spec%s" l)) (find_calls node.n_eqs)
  in
  fprintf ppf "@[<2>module Spec%s@\n%a@\n@\n@[<2>predicate spec %a %a =@\n%a@\n%a@]@\n@\n%a@]@\nend"
    node.n_name
    pp_import node
    var_pp_par node.n_input
    var_pp_par node.n_output
    var_exists node.n_local
    (pp_list_n "   /\\" spec_eq) node.n_eqs
    spec_proof_node (states, node)

let spec_file ppf (states, f) =
  fprintf ppf "%a" (pp_list_n "\n" (spec_node states)) f.f_nodes
