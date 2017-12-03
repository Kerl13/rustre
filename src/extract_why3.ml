open Ast_object

module E = struct
  open Pp_utils

  let build_filename a = a ^ ".mlw"

  let print_typedef ppf (i, ids) =
    fprintf ppf "type %s = %a" i (pp_list " | " (fun p -> fprintf p "%s")) ids

  let print_typedefs ppf typedefs =
    fprintf ppf "@[<h 2>module Types@\n%a@]@\nend" (pp_list_n "" print_typedef) typedefs

  let rec print_sty ppf (Sty ty) =
    fprintf ppf "%s" (match ty with
        | Ast_typed.TyBool -> "bool"
        | Ast_typed.TyNum Ast_typed.TyZ -> "int"
        | Ast_typed.TyNum Ast_typed.TyReal -> "float"
        | Ast_typed.TyEnum(n, _) -> "Types." ^ n
      )

  let rec print_expr: type a. 'b -> a oexpr -> unit = fun ppf e -> match e with
    | Ast_object.EVar (Var i | Loc i) -> fprintf ppf "%s" i
    | Ast_object.EVar (State s) -> fprintf ppf "state.%s" s
    | Ast_object.EConst c ->
      begin
        match c with
        | CBool a ->
          fprintf ppf "%b" a
        | CInt i ->
          fprintf ppf "%d" i
        | CReal i ->
          fprintf ppf "%f" i
        | CDataCons s ->
          fprintf ppf "Types.%s" s
      end
    | EBOp(a, b, c) -> fprintf ppf "%a %a %a" print_expr b Ast_typed.pp_bop a print_expr c
    | EUOp(a, b) -> fprintf ppf "%a %a" print_expr b Ast_typed.pp_uop a

  let rec filter_map: ('a -> 'b option) -> 'a list -> 'b list = fun f l ->
    match l with
    | [] -> []
    | t::q -> match f t with
      | Some s -> s :: filter_map f q
      | None -> filter_map f q

  let rec analyze_defs = function
    | SAssign { n; _} -> [n]
    | SSeq(a, b) -> analyze_defs a @ analyze_defs b
    | SReset(_) | SSkip -> []
    | SCall(_, _, _, res) -> res
    | SCase(_, b) ->
      List.hd b |> snd |> analyze_defs (* XXX alea jacta est *)

  let rec print_statement ppf (s: ostatement) = match s with
    | Ast_object.SAssign {n = (Var s | Loc s); expr } ->
      fprintf ppf "let %s = %a in" s print_expr expr
    | Ast_object.SAssign {n = State s; expr } ->
      fprintf ppf "state.%s <- %a;" s print_expr expr
    | Ast_object.SSeq (a,SSkip) ->
      print_statement ppf a
    | Ast_object.SSeq (a,b) ->
      fprintf ppf "%a@\n%a" print_statement a print_statement b
    | Ast_object.SSkip ->
      fprintf ppf ""
    | Ast_object.SCall (args, node, inst, result) ->
      fprintf ppf "let %a = Node%s.step state.%s %a in %a"
        (pp_list ", " (fun p s ->
             match s with
             | Var s | Loc s -> fprintf p "%s" s
             | State s -> fprintf p "state_%s" s)) result
        inst node
        (pp_list " " print_expr) (List.map (fun i -> EVar i) args)
        (pp_list_brk "" (fun p s ->
             match s with
             | State s -> fprintf p "state.%s := state_%s;" s s
             | _ -> assert false
           )) (List.filter (fun s -> match s with | State _ -> true | _ -> false) result)
    | Ast_object.SReset (mach, inst) ->
      fprintf ppf "Node%s.reset %s" mach inst
    | Ast_object.SCase (a, b) ->
      let vars = analyze_defs s
                 |> filter_map (function
                     | State _ -> None
                     | Var s | Loc s -> Some s) in

      fprintf ppf "@[<2>let @[<2>%a@] = match %a with@\n%a@]@\nend in"
        (pp_list_brk "," (fun ppf -> fprintf ppf "%s")) vars
        print_expr (EVar a)
        (pp_list_n "" (fun ppf (s, o) ->
             fprintf ppf "| %s -> @[<2>%a@ %a@]" s
               print_statement o(pp_list_brk "," (fun ppf -> fprintf ppf "%s")) vars))
        b


  let print_state ppf mach =
    if mach.memory = [] && mach.instances = [] then
      fprintf ppf "type state = unit"
    else
      fprintf ppf "type state = { @[%a %a@]}"
        (pp_list_brk "" (fun ppf (var, sty) ->
             fprintf ppf "mutable %s: %a;" var  print_sty sty)) mach.memory
        (pp_list_brk "" (fun ppf (i, m) ->
             fprintf ppf "%s: Node%s.state;" i m)) mach.instances

  let print_step ppf mach =
    let var_in, _, var_out, stat = mach.step in
    fprintf ppf "@[<2>let step (state:state) %a: (%a) =@\n%a@\n(%a)@]"
      (pp_list_brk " " (fun ppf (var, sty) ->
           fprintf ppf "(%s: %a)" var  print_sty sty)) var_in
      (pp_list_brk ", " (fun ppf (_, sty) ->
           print_sty ppf sty)) var_out
      print_statement stat
      (pp_list_brk ", " (fun ppf (var, _) ->
           fprintf ppf "%s" var)) var_out

  let print_reset ppf mach =
    fprintf ppf "@[<2>let reset (state:state): unit =@\n%a@\n()@]"
      print_statement mach.reset

  let print_machine ppf mach =
    fprintf ppf "@[<h 2>module Node%s@\nuse import int.Int@\nuse Types@\n%a@\n@\n%a@\n@\n%a@\n@\n%a@]@\n@\nend"
      mach.name
      (pp_list_n "" (fun ppf (_, m) -> fprintf ppf "use Node%s" m)) mach.instances
      print_state mach
      print_step mach
      print_reset mach

  let extract_to ppf f =
    fprintf ppf "%a\n@\n%a" print_typedefs f.objf_typedefs (pp_list_n "\n" print_machine) f.objf_machines
end
