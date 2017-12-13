open Ast_object
open Nil_analysis

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

  let print_bop: type a b. 'c -> (a, b) Ast_typed.binop -> unit =
    let open Ast_typed in
    fun ppf -> function
      | OpAdd -> fprintf ppf "+"
      | OpSub -> fprintf ppf "-"
      | OpMul -> fprintf ppf "*"
      | OpDiv -> assert false
      | OpMod -> assert false
      | OpLt -> fprintf ppf "<"
      | OpLe -> fprintf ppf "<="
      | OpGt -> fprintf ppf ">"
      | OpGe -> fprintf ppf ">="
      | OpEq -> fprintf ppf "="
      | OpNeq -> fprintf ppf "<>"
      | OpAnd -> fprintf ppf "&&"
      | OpOr -> fprintf ppf "||"
      | OpImpl -> assert false

  let rec print_expr: type a. ?prop:bool -> ?fonct:bool -> 'c -> a oexpr -> unit = fun ?(prop=false) ?(fonct=false) ppf e -> match e with
    | Ast_object.EVar (Var i | Loc i) -> fprintf ppf "%s" i
    | Ast_object.EVar (State s) ->
      if fonct && not prop then
        fprintf ppf "state_%s" s
      else
        fprintf ppf "state.%s" s
    | Ast_object.EConst c ->
      begin match c with
        | CBool a -> fprintf ppf "%B" a
        | CInt i -> fprintf ppf "%d" i
        | CReal i -> fprintf ppf "%f" i
        | CDataCons s -> fprintf ppf "%s" s
        | CNil Ast_typed.TyBool -> fprintf ppf "%B" true
        | CNil Ast_typed.TyNum Ast_typed.TyZ -> fprintf ppf "%d" 42
        | CNil Ast_typed.TyNum Ast_typed.TyReal -> fprintf ppf "%f" 0.
        | CNil Ast_typed.TyEnum (_, dcs) -> fprintf ppf "%s" (List.hd dcs)
      end
    | EBOp(Ast_typed.OpEq, b, c) when fonct -> fprintf ppf "(is_eq (%a) (%a))" (print_expr ~prop ~fonct) b (print_expr ~prop ~fonct) c
    | EBOp(Ast_typed.OpImpl, b, c) when prop -> fprintf ppf "((%a) -> (%a))" (print_expr ~prop ~fonct) b (print_expr ~prop ~fonct) c
    | EBOp(Ast_typed.OpImpl, b, c) -> fprintf ppf "((not (%a)) || (%a))" (print_expr ~prop ~fonct) b (print_expr ~prop ~fonct) c
    | EBOp(Ast_typed.OpMod, b, c) -> fprintf ppf "(mod (%a) (%a))" (print_expr ~prop ~fonct) b (print_expr ~prop ~fonct) c
    | EBOp(Ast_typed.OpDiv, b, c) -> fprintf ppf "(div (%a) (%a))" (print_expr ~prop ~fonct) b (print_expr ~prop ~fonct) c
    | EBOp(a, b, c) -> fprintf ppf "(%a %a %a)" (print_expr ~prop ~fonct) b print_bop a (print_expr ~prop ~fonct) c
    | EUOp(a, b) -> fprintf ppf "(%a %a)" Ast_typed.pp_uop a (print_expr ~prop ~fonct) b

  let rec print_statement ?(loc=fun s -> false) ?(prop = false) ?(fonct=false) ppf (s: ostatement) = match s with
    | Ast_object.SAssign {n = (Var s | Loc s); expr } when fonct && (loc s) ->
      fprintf ppf "%s = %a /\\ " s (print_expr ~prop ~fonct) expr
    | Ast_object.SAssign {n = (Var s | Loc s); expr } ->
      fprintf ppf "let %s = %a in" s (print_expr ~prop ~fonct) expr
    | Ast_object.SAssign {n = State s; expr } when prop ->
      fprintf ppf "state2.%s = %a /\\ " s (print_expr ~prop ~fonct) expr
    | Ast_object.SAssign {n = State s; expr } when fonct ->
      fprintf ppf "let state_%s = %a in" s (print_expr ~prop ~fonct) expr
    | Ast_object.SAssign {n = State s; expr } ->
      fprintf ppf "state.%s <- %a;" s (print_expr ~prop ~fonct) expr
    | Ast_object.SSeq (a,SSkip) ->
      print_statement ~prop ~loc ~fonct ppf a
    | Ast_object.SSeq (a,b) ->
      fprintf ppf "%a@\n%a" (print_statement ~prop ~loc ~fonct) a (print_statement ~prop ~loc ~fonct) b
    | Ast_object.SSkip ->
      fprintf ppf ""
    | Ast_object.SCall (args, node, inst, result) when fonct ->
      fprintf ppf "Node%s.step_fonct %a state.%s state2.%s /\\ "
        inst
        (pp_list " " (fun p s ->
             match s with
             | Var s | Loc s -> fprintf p "%s" s
             | State s ->
               assert false)) (args @ result)
        node node
    | Ast_object.SCall (args, node, inst, result) ->
      fprintf ppf "let ((%a)%a) = Node%s.step%a %a %a in %a"
        (pp_list ", " (fun p s ->
             match s with
             | Var s | Loc s -> fprintf p "%s" s
             | State s ->
               assert false; fprintf p "state.%s" s)) result
        (fun ppf s -> if fonct then fprintf ppf ", _, state_%s" s) node
        inst
        (fun ppf () -> if fonct then fprintf ppf "_fonct") ()
        (print_expr ~prop  ~fonct) (EVar (State node))
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
      if prop && List.for_all loc vars then
        begin
          fprintf ppf "@[<2>match %a with@\n%a@]@\nend /\\"
            (print_expr ~prop ~fonct) a
            (pp_list_n "" (fun ppf (s, o) ->
                 fprintf ppf "| %s -> @[<2>%a true@]" s
                   (print_statement ~prop ~loc ~fonct) o))
            b;
        end
      else if prop then
        begin
          fprintf ppf "@[<2>let (@[<2>%a@]) = match %a with@\n%a@]@\nend in"
            (pp_list_brk "," (fun ppf s -> if loc s then
                                 fprintf ppf "__%s" s
                               else fprintf ppf "%s" s)) vars
            (print_expr ~prop ~fonct) a
            (pp_list_n "" (fun ppf (s, o) ->
                 fprintf ppf "| %s -> @[<2>%a@ (%a)@]" s
                   (print_statement ~prop ~loc:(fun s -> false) ~fonct) o
                   (pp_list_brk "," (fun ppf -> fprintf ppf "%s")) vars))
            b;
          let vars = List.filter loc vars in
          (pp_list_brk " /\\" (fun ppf s -> fprintf ppf "__%s = %s" s s)) ppf vars;
        end
      else
        begin
          fprintf ppf "@[<2>let (@[<2>%a@]) = match %a with@\n%a@]@\nend in"
            (pp_list_brk "," (fun ppf -> fprintf ppf "%s")) vars
            (print_expr ~prop ~fonct) a
            (pp_list_n "" (fun ppf (s, o) ->
                 fprintf ppf "| %s -> @[<2>%a@ (%a)@]" s
                   (print_statement ~prop ~loc:(fun s -> false) ~fonct) o
                   (pp_list_brk "," (fun ppf -> fprintf ppf "%s")) vars))
            b;
        end


  let print_state ppf mach =
    if mach.memory = [] && mach.instances = [] then
      fprintf ppf "type state = unit"
    else
      fprintf ppf "type state = { @[%a %a@]}"
        (pp_list_brk "" (fun ppf (var, sty) ->
             fprintf ppf "mutable %s: %a;" var  print_sty sty)) mach.memory
        (pp_list_brk "" (fun ppf (i, m) ->
             fprintf ppf "%s: Node%s.state;" i m)) mach.instances

  let print_step ?(fonct=false) ppf (mach, _) =
    let var_in, _, var_out, stat = mach.step in
    let print_post ppf mach =
      fprintf ppf "ensures { @[let (%a) = result in@\n step_fonct %a (old state) state @] }"
        (pp_list_brk "," (fun ppf (var, _) ->
             fprintf ppf "%s" var)) var_out
        (pp_list_brk "" (fun ppf (var, _) ->
             fprintf ppf "%s" var)) (var_in @ var_out)
    in

    fprintf ppf "@[<2>let step (state:state) %a: (%a) @\n%a =@\n%a@\n(%a)@]"
      (pp_list_brk " " (fun ppf (var, sty) ->
           fprintf ppf "(%s: %a)" var  print_sty sty)) var_in
      (pp_list_brk ", " (fun ppf (_, sty) ->
           print_sty ppf sty)) var_out
      print_post mach
      (print_statement ~prop:false ~loc:(fun s -> false) ~fonct:false) stat
      (pp_list_brk ", " (fun ppf (var, _) ->
           fprintf ppf "%s" var)) var_out

  let rec get_result_var = function
    | SCall(_, _, _, res) -> res |> filter_map (function Loc s | Var s -> Some s | _ -> None)
    | SSeq(a, b) -> get_result_var a @ get_result_var b
    | SCase(_, l) -> List.map snd l |> List.map get_result_var |> List.concat
    | _ -> []

  exception Ok_found

  let rec print_ok_expr ppf = function
    | SAssign { n; expr } when n = Loc "ok" ->
      fprintf ppf "%a" (print_expr ~fonct:true ~prop:true) (Obj.magic expr); raise Ok_found
    | SAssign { n = Loc s; expr } -> ()
    | SSeq(a, b) ->
      print_ok_expr ppf a; print_ok_expr ppf b
    | SCase(e, l) as s ->
      let defs = analyze_defs ~fonct:true s in
      if List.mem (Loc "ok") defs then
        (fprintf ppf "@[<2>match %a with@\n%a@]@\nend"
           (print_expr ~prop:true ~fonct:true) e
           (pp_list_n "" (fun ppf (s, o) ->
                fprintf ppf "| %s -> @[<2>" s;
                try
                  print_ok_expr ppf o; assert false with
                | Ok_found ->
                fprintf ppf "@]@\n";)) l; raise Ok_found)
    | _ -> ()

  let print_step_fonct ppf (mach, _) =
    let var_in, _, var_out, stat = mach.step in
    let var_out_names = List.map fst var_out in
    let result_vars = get_result_var stat |> List.filter (fun a -> not @@ List.mem a var_out_names) in
    fprintf ppf "@[<2>predicate step_fonct %a %a (state:state) (state2:state) =@\n"
      (pp_list_brk " " (fun ppf (var, sty) ->
           fprintf ppf "(%s: %a)" var  print_sty sty)) var_in
      (pp_list_brk " " (fun ppf (var, sty) ->
           fprintf ppf "(%s:%a)" var print_sty sty)) var_out;
    if result_vars <> [] then
      fprintf ppf "exists %a.@\n"
        (pp_list_brk ", " (fun ppf var ->
             fprintf ppf "%s" var)) result_vars;

    (*if mach.memory = [] && mach.instances = [] then
      fprintf ppf ""
      else fprintf ppf "let { %a %a } = state in@\n"
        (pp_list_brk "" (fun ppf (var, _) ->
             fprintf ppf "%s = state_%s;" var var)) mach.memory
        (pp_list_brk "" (fun ppf (i, _) ->
             fprintf ppf "%s = state_%s; " i i)) mach.instances;*)
    fprintf ppf "%a@\n"
      (print_statement ~prop:true ~loc:(fun s' -> List.exists (fun (s,_) -> s = s') (var_in @ var_out)) ~fonct:true) stat;
    (*(pp_list_brk ", " (fun ppf (var, _) ->
           fprintf ppf "%s" var)) var_loc*)
    fprintf ppf " true@]@\n"

  let print_step_fonct_plus ppf (mach, _) =
    let var_in, _, var_out, stat = mach.step in
    let var_out_names = List.map fst var_out in
    let result_vars = get_result_var stat |> List.filter (fun a -> not (List.mem a var_out_names)) in
    fprintf ppf "@[<2>predicate step_fonct_ok %a %a (state:state) (state2:state) =@\n"
      (pp_list_brk " " (fun ppf (var, sty) ->
           fprintf ppf "(%s: %a)" var  print_sty sty)) var_in
      (pp_list_brk " " (fun ppf (var, sty) ->
           fprintf ppf "(%s:%a)" var print_sty sty)) var_out;
    if result_vars <> [] then
      fprintf ppf "exists %a.@\n"
        (pp_list_brk ", " (fun ppf var ->
             fprintf ppf "%s" var)) result_vars;

    (*if mach.memory = [] && mach.instances = [] then
      fprintf ppf ""
      else fprintf ppf "let { %a %a } = state in@\n"
        (pp_list_brk "" (fun ppf (var, _) ->
             fprintf ppf "%s = state_%s;" var var)) mach.memory
        (pp_list_brk "" (fun ppf (i, _) ->
             fprintf ppf "%s = state_%s; " i i)) mach.instances;*)
    fprintf ppf "%a@\n"
      (print_statement ~prop:true ~loc:(fun s' -> List.exists (fun (s,_) -> Format.printf "%s %s@." s s'; s = s') (var_in @ var_out)) ~fonct:true) stat;
    (*(pp_list_brk ", " (fun ppf (var, _) ->
           fprintf ppf "%s" var)) var_loc*)
    begin
      try
        print_ok_expr ppf stat; fprintf ppf "true"
      with
      | Ok_found -> ()
    end;
    fprintf ppf "@]"

  let print_reset_fonct ppf mach =
    fprintf ppf "@[<2>function reset_state : state =@\n%a@\n%a@]"
      (print_statement ~loc:(fun s -> false) ~prop:false ~fonct:true) mach.reset
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
      (print_statement ~loc:(fun s -> false) ~prop:false ~fonct:false) mach.reset

  let print_check_nil ppf mach =
    let var_in, _, _, step = mach.step in
    let var_in = List.map fst var_in in

    assert (ostatement_get_nils step = []);
    let nils = ostatement_get_nils mach.reset in
    let nils = List.map (function
        | State i -> i, "state_" ^ i
        | _ -> assert false) nils in
    let var_nils = List.map snd nils in
    fprintf ppf "@[<2>lemma nil_analysis: forall %a.@\nlet reset_state_nil = %a in@\nstep_fonct reset_state %a = step_fonct reset_state_nil %a@]"
      (pp_list_brk ", " (fun ppf s -> fprintf ppf "%s" s)) (var_in @ var_nils)
      (fun ppf () ->
         if nils = [] then fprintf ppf "reset_state"
         else fprintf ppf "{ reset_state with %a }"
             (pp_list_brk "" (fun ppf (i, s) -> fprintf ppf "%s = %s;" i s)) nils) ()
      (pp_list_brk "" (fun ppf s -> fprintf ppf "%s" s)) var_in
      (pp_list_brk "" (fun ppf s -> fprintf ppf "%s" s)) var_in

  let print_prop ppf (mach, var_loc) =
    let var_in, _, var_out, step = mach.step in
    let var_in = List.map fst var_in in
    let var_out = List.map fst var_out in
    fprintf ppf "@[<2>goal prop_init: forall %a, _s2.@\n"
      (pp_list_brk ", " (fun ppf s -> fprintf ppf "%s__1" s)) (var_in @ var_out);
    fprintf ppf "(step_fonct %a reset_state _s2"
      (pp_list_brk "" (fun ppf s -> fprintf ppf "%s__1" s)) (var_in @ var_out);
    fprintf ppf "-> step_fonct_ok %a reset_state _s2)@]@\n"
      (pp_list_brk "" (fun ppf s -> fprintf ppf "%s__1" s)) (var_in @ var_out);
    fprintf ppf "@[<2>goal prop_ind: forall %a, _s, _s2, _s3.@\n"
      (pp_list_brk ", " (fun ppf s -> fprintf ppf "%s__1, %s__2" s s)) (var_in @ var_out);
    fprintf ppf "(step_fonct_ok %a _s _s2 /\\ "
      (pp_list_brk "" (fun ppf s -> fprintf ppf "%s__1" s)) (var_in @ var_out);
    fprintf ppf "step_fonct %a _s2 _s3)@\n"
      (pp_list_brk "" (fun ppf s -> fprintf ppf "%s__2" s)) (var_in @ var_out);
    fprintf ppf "-> step_fonct_ok %a _s2 _s3@]@\n" (pp_list_brk "" (fun ppf s -> fprintf ppf "%s__2" s)) (var_in @ var_out)

  let print_machine locs ppf mach =
    let var_loc = List.assoc mach.name locs in
    (*let has_ok = List.exists (fun (s, _) -> s = "ok" ) (var_loc @ mach.var_out) in*)
    let has_ok = true in
    fprintf ppf "@[<h 2>module Node%s@\nuse import int.Int@\nuse import int.ComputerDivision@\nuse import Types@\n"
      mach.name;
    fprintf ppf "%a@\n"
      (pp_list_n "" (fun ppf (_, m) -> fprintf ppf "use Node%s" m)) mach.instances;
    fprintf ppf "%a@\n"
      print_state mach;
    fprintf ppf "%a@\n"
      print_step_fonct (mach, var_loc);
    fprintf ppf "%a@\n"
      print_step_fonct_plus (mach, var_loc);
    fprintf ppf "@\n%a@\n"
      print_reset_fonct mach;
    fprintf ppf "@\n%a"
      (print_step ~fonct:true) (mach, var_loc);
    fprintf ppf "@\n@\n%a"
      (print_reset ~fonct:true) mach;
    (*fprintf ppf "@\n%a"
      print_check_nil mach;*)
    if has_ok then
      fprintf ppf "@\n%a"
        print_prop (mach, var_loc);
    fprintf ppf "@]@\n@\nend"

  let extract_to ppf (f,_, locs) =
    fprintf ppf "%a\n@\n%a" print_typedefs f.objf_typedefs (pp_list_n "\n" (print_machine locs)) f.objf_machines
end
