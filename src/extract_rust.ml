(* not working for enums *)
(* create real main function... *)
open Ast_object

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
      fprintf ppf "((%a) %a (%a))" print_expr b pp_bop a print_expr c
    | EUOp (a, b) ->
      fprintf ppf "(%a %a)" pp_uop a print_expr b

  let rec print_statement ppf (s:ostatement) = match s with
    | Ast_object.SAssign { n = (Var s | Loc s); expr } ->
       fprintf ppf "%s = %a;" s print_expr expr
    | Ast_object.SAssign { n = State s; expr } ->
       fprintf ppf "self.%s = %a;" s print_expr expr
    | Ast_object.SSeq (a, SSkip) ->
       print_statement ppf a
    | Ast_object.SSeq (a, b) ->
       fprintf ppf "%a@\n%a" print_statement a print_statement b
    | Ast_object.SSkip ->
       fprintf ppf ""
    | Ast_object.SCall (args, node, inst, result) ->
       (* todo : optimize if List.length result < 2 *)
       fprintf ppf "{@[<4>@\nlet %a = self.%s.step(%a);@\n%a@]@\n}"
         (pp_type ", " (fun p s ->
              match s with
              | Var s | Loc s | State s -> fprintf p "tmp_%s" s)) result
         node
         (pp_list ", " print_expr) (List.map (fun i -> EVar i) args)
         (pp_list_n "" (fun p s ->
              match s with
              | Var s | Loc s -> fprintf p "%s = tmp_%s;" s s
              | State s -> fprintf p "self.%s = tmp_%s;" s s)) result
    | Ast_object.SReset (m, i) ->
       (* todo: no need for m ? *)
       fprintf ppf "self.%s.reset();" i
    | Ast_object.SCase (a, b) ->
       if ((List.length b = 2) && ((fst (List.hd b) = "True") || (fst (List.hd b) = "False"))) then
         (* we can reasonably assume that we're dealing with booleans *)
         fprintf ppf "@[<4>match %a {@\n%a@]}"
           print_expr a
           (pp_list_n "" (fun ppf (s, o) ->
                fprintf ppf "@[<4>%s => {%a@]}," (String.lowercase_ascii s) print_statement o)) b
       else
         fprintf ppf "match %a {@[<4>%a@]}"
           print_expr a
           (pp_list_n "" (fun ppf (s, o) ->
                fprintf ppf "%s => {@[<4>%a@]}," s print_statement o)) b



  let print_step ppf mach =
    let var_in, loc_vars, var_out, stat = mach.step in
    fprintf ppf "@[<4>pub fn step(&mut self, %a) -> %a {@\n%a@\n%a@\n@\n%a@]@\n}@\n"
      (* input variables:types *)
      (pp_list_brk ", " (fun ppf (var, sty) ->
           fprintf ppf "%s:%a" var print_sty sty)) var_in
      (* output types *)
      (pp_type ", " (fun ppf (_, sty) ->
           print_sty ppf sty)) var_out
      (* variable declarations *)
      (pp_list_n "" (fun ppf (var, sty) -> fprintf ppf "let mut %s:%a;" var print_sty sty)) (loc_vars@var_out)
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

  let print_use_typedefs ppf type_list =
    fprintf ppf "%a@\n"
      (pp_list_n "" print_use_typedef) type_list


  let print_machine ppf enum_list mach =
    fprintf ppf "@[<4>mod node_%s {@\n%a@\n%a@\n@\n%a@\n@[<4>impl Machine {@\n%a@\n%a@]@\n}@]@\n}"
      mach.name
      (* import of other instances (we need one import only per instance *)
      (pp_list_n "" (fun ppf m -> fprintf ppf "use node_%s;" m)) (List.sort_uniq Pervasives.compare (List.map (fun (_, m) -> m) mach.instances))
      (* import enum types *)
      print_use_typedefs enum_list
      (* definition of struct Machine *)
      print_state mach
      (* then implementation of reset and step *)
      print_step mach
      print_reset mach

  let print_main ppf main_node =
    (* todo: a real main *)
    fprintf ppf "pub fn main() {@\n    let mut mach:node_%s::Machine = Default::default();@\n    mach.reset();@\n    mach.step(true);@\n}" main_node


  let print_typedef ppf (i, l) =
    fprintf ppf "pub enum %a { %a }"
      print_enum i
      (pp_list ", " (fun ppf x -> print_datacons ppf x)) l

  let print_types ppf type_list =
    fprintf ppf "%a@\n"
      (pp_list_n "" print_typedef) type_list

  let extract_to ppf (f, main_node) =
    let enum_list = List.map (fun (x, _) -> x) f.objf_typedefs in
    fprintf ppf "%a@\n@\n%a@\n@\n%a@\n" print_types f.objf_typedefs (pp_list_n "\n" (fun ppf x -> print_machine ppf enum_list x)) f.objf_machines print_main main_node
end
