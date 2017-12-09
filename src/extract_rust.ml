(* todo: clean (functions with tuples...) *)
open Ast_object

let fprintf = Format.fprintf

module E = struct
  open Pp_utils

  let build_filename a = a ^ ".rs"

  (* In type t = A + B, enum is t, A is datacons *)
  let print_enum ppf n = fprintf ppf "Ty_%s" n
  let print_datacons ppf l = fprintf ppf "Dc_%s" l

  let print_sty ppf (Sty ty) =
    match ty with
    | Ast_typed.TyBool -> fprintf ppf "bool"
    | Ast_typed.TyNum Ast_typed.TyZ -> fprintf ppf "i64"
    | Ast_typed.TyNum Ast_typed.TyReal -> fprintf ppf "f64"
    | Ast_typed.TyEnum(n, _) -> fprintf ppf "%a" print_enum n

  let print_mach_inst pff m = fprintf pff "node_%s::Machine" m

  let print_state ppf mach =
    fprintf ppf "#[derive(Default)]@\n@[<4>pub struct Machine {@\n%a@\n%a@]@\n}@\n"
      (pp_list_n "" (fun ppf (var, sty) ->
           fprintf ppf "%s:%a," var print_sty sty)) mach.memory
      (pp_list_n "" (fun ppf (i, m) ->
           fprintf ppf "%s:%a," i print_mach_inst m)) mach.instances

  let pp_bop: type a b. 'c -> (a, b) Ast_typed.binop -> unit = fun ppf ->
    function
    | Ast_typed.OpAdd -> fprintf ppf "+"
    | Ast_typed.OpSub -> fprintf ppf "-"
    | Ast_typed.OpMul -> fprintf ppf "*"
    | Ast_typed.OpDiv -> fprintf ppf "/"
    | Ast_typed.OpMod -> fprintf ppf "%%"
    | Ast_typed.OpLt -> fprintf ppf "<"
    | Ast_typed.OpLe -> fprintf ppf "<="
    | Ast_typed.OpGt -> fprintf ppf ">"
    | Ast_typed.OpGe -> fprintf ppf ">="
    | Ast_typed.OpEq -> fprintf ppf "=="
    | Ast_typed.OpNeq -> fprintf ppf "!="
    | Ast_typed.OpAnd -> fprintf ppf "&"
    | Ast_typed.OpOr -> fprintf ppf "|"
    | Ast_typed.OpImpl -> failwith "rust / pp_bop / OpImpl : not implemented"

  let pp_uop: type a b. 'c -> (a, b) Ast_typed.unop -> unit = fun ppf ->
    function
    | Ast_typed.OpNot -> fprintf ppf "!"
    | Ast_typed.OpUMinus -> fprintf ppf "-"

  let pp_type sep pp ppf l =
    if List.length l < 2 then
      fprintf ppf "%a" (pp_list sep pp) l
    else
      fprintf ppf "(%a)" (pp_list sep pp) l


  let rec print_expr: type a. Format.formatter -> a oexpr -> unit = fun ppf e ->
    match e with
    | EVar (Var i | Loc i) -> fprintf ppf "%s" i
    | EVar (State s) -> fprintf ppf "self.%s" s
    | EConst c ->
       begin
         match c with
         | CBool a ->
            fprintf ppf "%b" a
         | CInt i ->
            fprintf ppf "%d" i
         | CReal i ->
            fprintf ppf "%f" i
         | CDataCons s ->
            print_datacons ppf s
       end
    | EBOp (a, b, c) ->
      fprintf ppf "(%a %a %a)" print_expr b pp_bop a print_expr c
    | EUOp (a, b) ->
      fprintf ppf "(%a %a)" pp_uop a print_expr b

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

  let rec print_statement ppf (s:ostatement) = match s with
    | Ast_object.SAssign { n = (Var s | Loc s); expr } ->
       fprintf ppf "let %s = %a;" s print_expr expr
    | Ast_object.SAssign { n = State s; expr } ->
       fprintf ppf "self.%s = %a;" s print_expr expr
    | Ast_object.SSeq (a, SSkip) ->
       print_statement ppf a
    | Ast_object.SSeq (a, b) ->
       fprintf ppf "%a@\n%a" print_statement a print_statement b
    | Ast_object.SSkip ->
       fprintf ppf ""
    | Ast_object.SCall (args, node, _, result) ->
       (* todo : optimize if List.length result < 2 *)
       fprintf ppf "let %a = self.%s.step(%a);@\n"
         (pp_type ", " (fun p s ->
              match s with
              | Var s | Loc s -> fprintf p "%s" s
              | State _ -> failwith "extract_rust: weird case")) result
         node
         (pp_list ", " print_expr) (List.map (fun i -> EVar i) args)
       (* fprintf ppf "{@[<4>@\nlet %a = self.%s.step(%a);@\n%a@]@\n}"
        *   (pp_type ", " (fun p s ->
        *        match s with
        *        | Var s | Loc s | State s -> fprintf p "tmp_%s" s)) result
        *   node
        *   (pp_list ", " print_expr) (List.map (fun i -> EVar i) args)
        *   (pp_list_n "" (fun p s ->
        *        match s with
        *        | Var s | Loc s -> fprintf p "%s = tmp_%s;" s s
        *        | State s -> fprintf p "self.%s = tmp_%s;" s s)) result *)
    | Ast_object.SReset (_m, i) ->
       (* todo: no need for m ? *)
       fprintf ppf "self.%s.reset();" i
    | Ast_object.SCase (a, b) ->
       (* todo: pourquoi on ne prend pas les state ? *)
       (* todo: copie propre / merge avec le truc de Lucas *)
       let vars = analyze_defs s
                  |> filter_map (function
                         | State _ -> None
                         | Var s | Loc s -> Some s) in
       let vars = if List.length vars = 0 then ["()"] else vars in
       if ((List.length b = 2) && ((fst (List.hd b) = "True") || (fst (List.hd b) = "False"))) then
         fprintf ppf "@[<2>let %a = match %a {@\n%a@]};"
           (pp_list_brk ", " (fun ppf -> fprintf ppf "%s")) vars
           print_expr a
           (pp_list_n "" (fun ppf (s, o) ->
                fprintf ppf "%s => {@[<4>%a@ (%a)@]}"
                  (String.lowercase_ascii s)
                  print_statement o
                  (pp_list_brk "," (fun ppf -> fprintf ppf "%s")) vars)) b
       else
         fprintf ppf "@[<2>let %a = match %a {@\n%a@]};"
           (pp_list_brk ", " (fun ppf -> fprintf ppf "%s")) vars
           print_expr a
           (pp_list_n "" (fun ppf (s, o) ->
                fprintf ppf "%a => {@[<4>%a@ (%a)@]}"
                  print_datacons s
                  print_statement o
                  (pp_list_brk "," (fun ppf -> fprintf ppf "%s")) vars)) b
       (* if ((List.length b = 2) && ((fst (List.hd b) = "True") || (fst (List.hd b) = "False"))) then
        *   (\* we can reasonably assume that we're dealing with booleans *\)
        *   fprintf ppf "@[<4>match %a {@\n%a@]}"
        *     print_expr a
        *     (pp_list_n "" (fun ppf (s, o) ->
        *          fprintf ppf "@[<4>%s => {%a@]}," (String.lowercase_ascii s) print_statement o)) b
        * else
        *   fprintf ppf "match %a {@[<4>%a@]}"
        *     print_expr a
        *     (pp_list_n "" (fun ppf (s, o) ->
        *          fprintf ppf "%s => {@[<4>%a@]}," s print_statement o)) b *)



  let print_step ppf mach =
    let var_in, _, var_out, stat = mach.step in
    fprintf ppf "@[<4>pub fn step(&mut self, %a) -> %a {@\n%a@\n@\n%a@]@\n}@\n"
      (* input variables:types *)
      (pp_list ", " (fun ppf (var, sty) ->
           fprintf ppf "%s:%a" var print_sty sty)) var_in
      (* output types *)
      (pp_type ", " (fun ppf (_, sty) ->
           print_sty ppf sty)) var_out
      (* variable declarations *)
      (* (pp_list_n "" (fun ppf (var, sty) -> fprintf ppf "let mut %s:%a;" var print_sty sty)) (loc_vars@var_out) *)
      (* statements *)
      print_statement stat
      (* output variables *)
      (pp_type ", " (fun ppf (var, _) ->
           fprintf ppf "%s" var)) var_out


  let print_reset ppf mach =
    fprintf ppf "@[<4>pub fn reset(&mut self) {@\n%a@]@\n}@\n" print_statement mach.reset

  let print_use_typedef ppf i =
    fprintf ppf "use %a;@\nuse %a::*;@\n"
      print_enum i
      print_enum i

  let print_use_typedefs ppf typedefs =
    let type_list = List.map (fun (x, _) -> x) typedefs in
    fprintf ppf "%a@\n"
      (pp_list_n "" print_use_typedef) type_list


  let print_machine ppf typedefs mach =
    fprintf ppf "@[<4>mod node_%s {@\n%a@\n%a@\n@\n%a@\n@[<4>impl Machine {@\n%a@\n%a@]@\n}@]@\n}"
      mach.name
      (* import of other instances (we need one import only per instance *)
      (pp_list_n "" (fun ppf m -> fprintf ppf "use node_%s;" m)) (List.sort_uniq Pervasives.compare (List.map (fun (_, m) -> m) mach.instances))
      (* import enum types *)
      print_use_typedefs typedefs
      (* definition of struct Machine *)
      print_state mach
      (* then implementation of reset and step *)
      print_step mach
      print_reset mach

  let print_ask_input ppf vars_in =
    fprintf ppf "if@ !silent@ {println!(\"Input: %a\")};@\n"
      (pp_list ", " (fun ppf (var, sty) ->
           fprintf ppf "%s:%a" var print_sty sty)) vars_in

  let print_parse_type ppf (typedefs, n, Sty ty) =
    match ty with
    | Ast_typed.TyEnum(ty_enum, _) ->
       let enum_name, enum_dcons = List.find (fun (x, _) -> x = ty_enum) typedefs in
       fprintf ppf "@[<4>let arg%d = match splitted[%d] {@\n%a@\n%s@]@\n};"
         n n
         (pp_list_n "" (fun ppf x ->
              fprintf ppf "\"%s\" => %a," enum_name print_datacons x)) enum_dcons
         "_ => continue"
    | _ ->
       fprintf ppf "@[<4>let arg%d = match splitted[%d].parse::<%a>() {@\nOk(i) => i,@\nErr(..) => continue@]@\n};"
         n n print_sty (Sty ty)


  let rec print_parse_types ppf (typedefs, n, l) =
    match l with
    | [] ->
       fprintf ppf ""
    | x::xs ->
       fprintf ppf "%a@\n%a"
         print_parse_type (typedefs, n, snd x)
         print_parse_types (typedefs, n+1, xs)


  let print_parse_args ppf (typedefs, var_in) =
    fprintf ppf "use std::io;@\n" ;
    fprintf ppf "use std::env;@\n" ;
    fprintf ppf "@[<4>fn parse_args(silent: bool) -> %a {@\n%a@\n@[<4>loop {@\n%s@\n%a@\n%s@\n%s@\n%a@\nreturn %a;@]@\n}@]@\n}@\n"
      (* output types *)
      (pp_type ", " (fun ppf (_, sty) ->
           print_sty ppf sty)) var_in
      (* import of enum types *)
      print_use_typedefs typedefs
      "let mut input_text = String::new();"
      (* ask for input values *)
      print_ask_input var_in
      "io::stdin().read_line(&mut input_text).expect(\"failed to read from stdin\");"
      "let splitted:Vec<&str> = input_text.trim().split(' ').collect();"
      (* let arg_i = match ....parse::<...> *)
      print_parse_types (typedefs, 0, var_in)
      (* (arg_0, ..., arg_N) *)
      (pp_type ", " (fun ppf x -> fprintf ppf "%s" x)) (List.mapi (fun i (_, _) -> "arg" ^ (string_of_int i)) var_in)

  let print_typedef ppf (i, l) =
    fprintf ppf "#[derive(Clone, Copy)]@\npub enum %a { %a }@\nimpl Default for %a { fn default() -> %a { %a::%a }}@\n"
      print_enum i
      (pp_list ", " (fun ppf x -> print_datacons ppf x)) l
      print_enum i
      print_enum i
      print_enum i
      print_datacons (List.hd l)

  let print_types ppf type_list =
    fprintf ppf "%a@\n"
      (pp_list_n "" print_typedef) type_list

  let print_main ppf (typedefs, main_machine) =
    let (vars_in, _, _, _) = main_machine.step in
    fprintf ppf "%a@\n@[<4>pub fn main() {@\n" print_parse_args (typedefs, vars_in) ;
    fprintf ppf "let mut mach:node_%s::Machine = Default::default();@\n" main_machine.name;
    fprintf ppf "let args: Vec<_> = env::args().collect();@\n" ;
    fprintf ppf "@[<4>let silent =@\n" ;
    fprintf ppf "@[<4>if@ (args.len() > 1)@ &&@ args[1] == \"-silent\"@ {@\ntrue@]@\n}@ @[<2>else@ {@\nfalse@]@\n}@];@\n" ;
    fprintf ppf "mach.reset();@\n@[<4>loop {@\nlet %a = parse_args(silent);@\nprintln!(\"{:?}\", mach.step(%a));@]@\n}@]@\n}"
      (* (arg0, ..., argN) *)
      (pp_type ", " (fun ppf x -> fprintf ppf "%s" x)) (List.mapi (fun i (_, _) -> "arg" ^ (string_of_int i)) vars_in)
      (* arg0, ..., argN *)
      (pp_list ", " (fun ppf x -> fprintf ppf "%s" x)) (List.mapi (fun i (_, _) -> "arg" ^ (string_of_int i)) vars_in)


  let extract_to ppf (f, main_node) =
    let main_machine = List.find (fun m -> m.name = main_node) f.objf_machines in
    fprintf ppf "%a@\n@\n%a@\n@\n%a@\n" print_types f.objf_typedefs (pp_list_n "\n" (fun ppf x -> print_machine ppf f.objf_typedefs x)) f.objf_machines print_main (f.objf_typedefs, main_machine)
end
