open Ast_object

module E = struct
  open Pp_utils

  let fprintf = Format.fprintf

  let build_filename a = a ^ ".mlw"

  let print_typedef ppf (i, ids) =
    fprintf ppf "type %s = %a" i (pp_list " | " (fun p -> fprintf p "%s")) ids

  let prelude ppf () =
    fprintf ppf "function is_eq (a:'a) (b:'a): bool = (a = b)"

  let print_typedefs ppf typedefs =
    fprintf ppf "@[<h 2>module Types@\n%a@\n%a@]@\nend" prelude () (pp_list_n "" print_typedef) typedefs

  let print_sty ppf (Sty ty) =
    fprintf ppf "%s" (match ty with
        | Ast_typed.TyBool -> "bool"
        | Ast_typed.TyNum Ast_typed.TyZ -> "int"
        | Ast_typed.TyNum Ast_typed.TyReal -> "float"
        | Ast_typed.TyEnum(n, _) -> "Types." ^ n
      )

  let rec print_expr: type a. ?fonct:bool -> 'c -> a oexpr -> unit = fun ?(fonct=false) ppf e -> match e with
    | Ast_object.EVar (Var i | Loc i) -> fprintf ppf "%s" i
    | Ast_object.EVar (State s) ->
      if fonct then
        fprintf ppf "state_%s" s
      else
        fprintf ppf "state.%s" s
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
    | EBOp(Ast_typed.OpEq, b, c) when fonct -> fprintf ppf "Types.is_eq (%a) (%a)" (print_expr ~fonct) b (print_expr ~fonct) c
    | EBOp(a, b, c) -> fprintf ppf "%a %a %a" (print_expr ~fonct) b Ast_typed.pp_bop a (print_expr ~fonct) c
    | EUOp(a, b) -> fprintf ppf "%a %a" (print_expr ~fonct) b Ast_typed.pp_uop a

  let rec filter_map: ('a -> 'b option) -> 'a list -> 'b list = fun f l ->
    match l with
    | [] -> []
    | t::q -> match f t with
      | Some s -> s :: filter_map f q
      | None -> filter_map f q

  let rec analyze_defs ?(fonct=false) = function
    | SAssign { n; _} -> [n]
    | SSeq(a, b) -> analyze_defs ~fonct a @ analyze_defs ~fonct b
    | SReset(a, b) -> if fonct then [State ("state_" ^ b)] else []
    | SSkip -> []
    | SCall(_, _, i, res) ->
      if fonct then
        res
      else
        (State ("state_" ^ i)) :: res
    | SCase(_, b) ->
      List.map snd b |> List.map (analyze_defs ~fonct) |> List.concat |> List.sort_uniq compare (* XXX alea jacta est *)

  let rec print_statement ?(fonct=false) ppf (s: ostatement) = match s with
    | Ast_object.SAssign {n = (Var s | Loc s); expr } ->
      fprintf ppf "let %s = %a in" s (print_expr ~fonct) expr
    | Ast_object.SAssign {n = State s; expr } when fonct ->
      fprintf ppf "let state_%s = %a in" s (print_expr ~fonct) expr
    | Ast_object.SAssign {n = State s; expr } ->
      fprintf ppf "state.%s <- %a;" s (print_expr ~fonct) expr
    | Ast_object.SSeq (a,SSkip) ->
      print_statement ~fonct ppf a
    | Ast_object.SSeq (a,b) ->
      fprintf ppf "%a@\n%a" (print_statement ~fonct) a (print_statement ~fonct) b
    | Ast_object.SSkip ->
      fprintf ppf ""
    | Ast_object.SCall (args, node, inst, result) ->
      fprintf ppf "let ((%a)%a) = Node%s.step%a %a %a in %a"
        (pp_list ", " (fun p s ->
             match s with
             | Var s | Loc s -> fprintf p "%s" s
             | State s ->
               assert false; fprintf p "state.%s" s)) result
        (fun ppf s -> if fonct then fprintf ppf ", state_%s" s) node
        inst
        (fun ppf () -> if fonct then fprintf ppf "_fonct") ()
        (print_expr ~fonct) (EVar (State node))
        (pp_list " " (print_expr ~fonct)) (List.map (fun i -> EVar i) args)
        (pp_list_brk "" (fun p s ->
             match s with
             | State s -> fprintf p "state.%s := state_%s;" s s
             | _ -> assert false
           )) (List.filter (fun s -> match s with | State _ -> true | _ -> false) result)
    | Ast_object.SReset (mach, inst) ->
      if fonct then
        fprintf ppf "let state_%s = Node%s.reset_state in" inst mach
      else
        fprintf ppf "Node%s.reset state.%s;" mach inst
    | Ast_object.SCase (a, b) ->
      let vars = analyze_defs ~fonct s
                 |> filter_map (function
                     | State s -> Some s
                     | Var s | Loc s -> Some s) in

      fprintf ppf "@[<2>let (@[<2>%a@]) = match %a with@\n%a@]@\nend in"
        (pp_list_brk "," (fun ppf -> fprintf ppf "%s")) vars
        (print_expr ~fonct) a
        (pp_list_n "" (fun ppf (s, o) ->
             fprintf ppf "| %s -> @[<2>%a@ (%a)@]" s
               (print_statement ~fonct) o
               (pp_list_brk "," (fun ppf -> fprintf ppf "%s")) vars))
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

  let print_step ?(fonct=false) ppf (mach, var_loc) =
    let var_in, _, var_out, stat = mach.step in
    let print_post ppf mach =
      fprintf ppf "ensures { let ((%a), sta) = step_fonct (old state) %a in@\n(%a) = result /\\@ state = sta }"
        (pp_list_brk "," (fun ppf (var, _) ->
             fprintf ppf "%s" var)) (var_out @ (List.map (fun (_, ty) -> "_", ty) var_loc))
        (pp_list_brk "" (fun ppf (var, _) ->
             fprintf ppf "%s" var)) var_in
        (pp_list_brk "," (fun ppf (var, _) ->
             fprintf ppf "%s" var)) var_out
    in

    fprintf ppf "@[<2>let step (state:state) %a: (%a) @\n%a =@\n%a@\n(%a)@]"
      (pp_list_brk " " (fun ppf (var, sty) ->
           fprintf ppf "(%s: %a)" var  print_sty sty)) var_in
      (pp_list_brk ", " (fun ppf (_, sty) ->
           print_sty ppf sty)) var_out
      print_post mach
      (print_statement ~fonct:false) stat
      (pp_list_brk ", " (fun ppf (var, _) ->
           fprintf ppf "%s" var)) var_out

  let print_step_fonct ppf (mach, var_loc) =
    let var_in, _, var_out, stat = mach.step in
    fprintf ppf "@[<2>function step_fonct (state:state) %a: ((%a), state) =@\n%a%a@\n((%a), %a)@]"
      (pp_list_brk " " (fun ppf (var, sty) ->
           fprintf ppf "(%s: %a)" var  print_sty sty)) var_in
      (pp_list_brk ", " (fun ppf (_, sty) ->
           print_sty ppf sty)) (var_out @ var_loc)
      (fun ppf mach ->
         if mach.memory = [] && mach.instances = [] then
           fprintf ppf ""
         else fprintf ppf "let { %a %a } = state in@\n"
             (pp_list_brk "" (fun ppf (var, _) ->
                  fprintf ppf "%s = state_%s;" var var)) mach.memory
             (pp_list_brk "" (fun ppf (i, _) ->
                  fprintf ppf "%s = state_%s; " i i)) mach.instances) mach
      (print_statement ~fonct:true) stat
      (pp_list_brk ", " (fun ppf (var, _) ->
           fprintf ppf "%s" var)) (var_out @ var_loc)
      (fun ppf mach ->
         if mach.memory = [] && mach.instances = [] then
           fprintf ppf "()"
         else fprintf ppf "{ %a %a }"
             (pp_list_brk "" (fun ppf (var, _) ->
                  fprintf ppf "%s = state_%s;" var var)) mach.memory
             (pp_list_brk "" (fun ppf (i, _) ->
                  fprintf ppf "%s = state_%s; " i i)) mach.instances) mach

  let print_reset_fonct ppf mach =
    fprintf ppf "@[<2>function reset_state : state =@\n%a@\n%a@]"
      (print_statement ~fonct:true) mach.reset
      (fun ppf mach ->
         if mach.memory = [] && mach.instances = [] then
           fprintf ppf "()"
         else fprintf ppf "{ %a %a }"
             (pp_list_brk "" (fun ppf (var, _) ->
                  fprintf ppf "%s = state_%s;" var var)) mach.memory
             (pp_list_brk "" (fun ppf (i, _) ->
                  fprintf ppf "%s = state_%s; " i i)) mach.instances) mach

  let print_reset ?(fonct = true) ppf mach =
    let print_post ppf () =
      fprintf ppf "ensures { state = reset_state }" in
    fprintf ppf "@[<2>let reset (state:state): unit @\n%a =@\n%a@\n()@]"
      print_post ()
      (print_statement ~fonct:false) mach.reset

  let print_machine locs ppf mach =
    let var_loc = List.assoc mach.name locs in
    fprintf ppf "@[<h 2>module Node%s@\nuse import int.Int@\nuse Types@\n%a@\n%a@\n%a@\n@\n%a@\n@\n%a@\n@\n%a@]@\n@\nend"
      mach.name
      (pp_list_n "" (fun ppf (_, m) -> fprintf ppf "use Node%s" m)) mach.instances
      print_state mach
      print_step_fonct (mach, var_loc)
      print_reset_fonct mach
      (print_step ~fonct:true) (mach, var_loc)
      (print_reset ~fonct:true) mach

  let extract_to ppf (f,_, locs) =
    fprintf ppf "%a\n@\n%a" print_typedefs f.objf_typedefs (pp_list_n "\n" (print_machine locs)) f.objf_machines
end
