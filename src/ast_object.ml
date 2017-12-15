type machine_id = string
type var_id = string
type z = int

open Ast_typed

type sty = Sty: 'a Ast_typed.ty -> sty

type var_list = (var_id * sty) list

type machine_ty = Ast_parsing.ty list * Ast_parsing.ty list
type instance = string
type memory = var_list

type ident = Var of var_id | State of var_id | Loc of var_id

type enum = Ast_typed.enum

type _ oconst =
  | CBool : bool -> bool oconst
  | CInt  : int -> int num_ty oconst
  | CReal : float -> float num_ty oconst
  | CDataCons : Ast_typed.ident -> enum oconst
  | CNil: 'a Ast_typed.ty -> 'a oconst

type wconst = Const: 'a oconst -> wconst

type _ oexpr =
  | EVar      : ident -> 'a oexpr
  | EConst    : 'a oconst -> 'a oexpr
  | EBOp      : ('a, 'b) binop * 'a oexpr * 'a oexpr -> 'b oexpr
  | EUOp      : ('a, 'b) unop * 'a oexpr -> 'b oexpr

type ostatement =
  | SAssign : { n: ident; expr: 'a oexpr } -> ostatement
  | SSeq of ostatement * ostatement
  | SSkip
  | SCall of ident list * instance * machine_id * ident list (* args, node name, result vars *)
  | SReset of machine_id * instance
  | SCase : 'a oexpr * (string * ostatement) list -> ostatement (* Constructors are numbered, the nth
                                                   statement corresponds to the nth
                                                   constructor -- 2 in case of Booleans *)

type machine = {
  name: string;
  memory: memory;
  instances: (instance * machine_id) list;
  reset: ostatement;
  step: var_list * var_list * var_list * ostatement; (* in, tmp var, out *)
}

type file = {
  objf_typedefs : (Ast_typed.ident * Ast_typed.ident list) list ;
  objf_machines : machine list
}




(**
 * Pretty printer
 **)

open Pp_utils



let pp_oconst: type a. 'b -> a oconst -> unit = fun ppf -> function
  | CInt n -> fprintf ppf "%d" n
  | CReal f -> fprintf ppf "%.16g" f
  | CBool b -> fprintf ppf "%B" b
  | CDataCons dc -> fprintf ppf "%s" dc
  | CNil _ -> fprintf ppf "nil"

let rec pp_expr: type a. 'c -> a oexpr -> unit = fun ppf -> function
  | EConst c -> fprintf ppf "%a" pp_oconst c
  | EVar (Var i) -> fprintf ppf "%s" i
  | EVar (State i) -> fprintf ppf "state(%s)" i
  | EVar (Loc i) -> fprintf ppf "~%s" i
  | EBOp(a, b, c) -> fprintf ppf "%a %a %a" pp_expr b pp_bop a pp_expr c
  | EUOp(a, b) -> fprintf ppf "%a %a" pp_expr b pp_uop a

let rec pp_ostatement ppf = function
  | SAssign { n; expr } ->
    fprintf ppf "%a := %a;" pp_expr (EVar n) pp_expr expr
  | SSeq(a, SSkip) -> pp_ostatement ppf a
  | SSeq(a, b) -> fprintf ppf "%a@\n%a" pp_ostatement a pp_ostatement b
  | SSkip -> fprintf ppf "skip;"
  | SCall(args, inst, mach, ids) ->
    fprintf ppf "(%a) := %s(%a with %s);" (pp_list ", " pp_expr) (List.map (fun i -> EVar i) ids)
      mach
      (pp_list ", " pp_expr) (List.map (fun i -> EVar i) args)
      inst

  | SReset(machine_id, inst) ->
    fprintf ppf "%s.reset() with %s;" machine_id inst
  | SCase(i, args) ->
    fprintf ppf "@[<h 2>case %a {%a@]@\n}"
      pp_expr i
      (pp_list "" (fun ppf (s, i) -> fprintf ppf "@\n@[<h 2>%s -> {@\n%a@]@\n}" i pp_ostatement s))
      (List.mapi (fun _ (i, s) -> (s, i)) args)


let pp_sty: type a. 'b -> a Ast_normalized.sty -> unit = fun ppf -> function
  | Ast_normalized.StyBool -> fprintf ppf "bool"
  | Ast_normalized.StyNum TyZ -> fprintf ppf "int"
  | Ast_normalized.StyNum TyReal -> fprintf ppf "real"

let pp_machine ppf m =
  let var_in, var_tmp, var_out, stmt = m.step in
  fprintf ppf "@[<2>machine %s {@\n@[<h 2>memory: %a@]@\n@[<h 2>instances: %a@]@\n@[<h 2>reset () {@\n%a@]@\n}@\n@[<h 2>step (%a): %a {@\nvar %a in@\n%a@]@\n}@]@\n}@."
    m.name
    (pp_list ", " (fun ppf (s, Sty ty) -> fprintf ppf "%s:%a" s pp_ty ty)) m.memory
    (pp_list ", " (fun ppf (s, mach) -> fprintf ppf "%s:%s" s mach)) m.instances
    pp_ostatement m.reset
    (pp_list ", " (fun ppf (s, Sty ty) -> fprintf ppf "%s:%a" s pp_ty ty)) var_in
    (pp_list ", " (fun ppf (s, Sty ty) -> fprintf ppf "%s:%a" s pp_ty ty)) var_out
    (pp_list ", " (fun ppf (s, Sty ty) -> fprintf ppf "%s:%a" s pp_ty ty)) var_tmp
    pp_ostatement stmt

let pp_file fmt file =
  fprintf fmt "%a@\n" (pp_list_n "\n" Ast_parsing.pp_typedef) file.objf_typedefs ;
  fprintf fmt "%a" (pp_list "" pp_machine) file.objf_machines

let sty_to_ty: type a. a Ast_typed.ty -> Ast_parsing.ty = fun ty ->
  match ty with
  | Ast_typed.TyBool -> Ast_parsing.TyBool
  | Ast_typed.TyNum Ast_typed.TyZ -> Ast_parsing.TyInt
  | Ast_typed.TyNum Ast_typed.TyReal -> Ast_parsing.TyReal
  | Ast_typed.TyEnum (a, b) -> Ast_parsing.TyEnum (a,b)

type state_component = State_var of string * Ast_parsing.ty | Mach_var of string * string

let extract_states file =
  List.map (fun mach ->
      let vars = List.map (fun (i, Sty sty) ->
          State_var (i, sty_to_ty sty) ) mach.memory in
      let machs = List.map (fun (i, mach) ->
          Mach_var (i, mach)) mach.instances in
      mach.name, vars @ machs) file.objf_machines
