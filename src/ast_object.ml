type machine_id = string
type var_id = string
type z = int

open Ast_typed

type sty = Sty: 'a Ast_normalized.sty -> sty

type var_list = (var_id * sty) list

type machine_ty = Ast_parsing.ty list * Ast_parsing.ty list
type instance = machine_id
type memory = var_list

type ident = Var of var_id | State of var_id

type _ oconst =
  | CBool : bool -> bool oconst
  | CInt  : int -> int num_ty oconst
  | CReal : float -> float num_ty oconst

type _ oexpr =
  | EVar      : ident -> 'a oexpr
  | EConst    : 'a oconst -> 'a oexpr
  | EBOp      : ('a, 'b) binop * 'a oexpr * 'a oexpr -> 'b oexpr
  | EUOp      : ('a, 'b) unop * 'a oexpr -> 'b oexpr

type ostatement =
  | SAssign of { n: ident; expr: 'a. 'a oexpr }
  | SSeq of ostatement * ostatement
  | SSkip
  | SCall of ident list * machine_id * ident list
  | SReset of machine_id
  | SCase of ident * (ostatement list) (* Constructors are numbered, the nth
                                          statement corresponds to the nth
                                          constructor -- 2 in case of Booleans *)

type machine = {
  name: string;
  memory: memory;
  instances: instance list;
  reset: ostatement;
  step: var_list * ostatement;
}

type file = machine list




(**
 * Pretty printer
 **)

(* XXX add some boxes here and there *)

let fprintf = Format.fprintf

let rec pp_list sep pp ppf = function
  | [] -> fprintf ppf ""
  | [x] -> fprintf ppf "%a" pp x
  | x :: xs -> fprintf ppf "%a%s%a" pp x sep (pp_list sep pp) xs



let pp_oconst: type a. 'b -> a oconst -> unit = fun ppf -> function
  | CInt n -> fprintf ppf "%d" n
  | CReal f -> fprintf ppf "%f" f
  | CBool b -> fprintf ppf "%B" b

let rec pp_expr: type a. 'c -> a oexpr -> unit = fun ppf -> function
  | EConst c -> fprintf ppf "%a" pp_oconst c
  | EVar (Var i) -> fprintf ppf "%s" i
  | EVar (State i) -> fprintf ppf "state(%s)" i
  | EBOp(a, b, c) -> fprintf ppf "%a %a %a" pp_expr b pp_bop a pp_expr c
  | EUOp(a, b) -> fprintf ppf "%a %a" pp_expr b pp_uop a

let rec pp_ostatement ppf = function
  | SAssign { n; expr } ->
    fprintf ppf "%a := %a;" pp_expr (EVar n) pp_expr expr
  | SSeq(a, b) -> fprintf ppf "%a\n%a" pp_ostatement a pp_ostatement b
  | SSkip -> fprintf ppf "skip;"
  | SCall(ids, mach, args) ->
    fprintf ppf "(%a) := %s(%a);" (pp_list ", " pp_expr) (List.map (fun i -> EVar i) ids)
      mach
      (pp_list ", " pp_expr) (List.map (fun i -> EVar i) args)

  | SReset(machine_id) ->
    fprintf ppf "%s.reset();" machine_id
  | SCase(i, args) ->
    fprintf ppf "case %a {\n%a\n}"
      pp_expr (EVar i)
      (pp_list "\n" (fun ppf (s, i) -> fprintf ppf "%d -> {\n%a\n}" i pp_ostatement s))
      (List.mapi (fun i s -> (s, i)) args)


let pp_sty: type a. 'b -> a Ast_normalized.sty -> unit = fun ppf -> function
  | Ast_normalized.StyBool -> fprintf ppf "bool"
  | Ast_normalized.StyNum TyZ -> fprintf ppf "int"
  | Ast_normalized.StyNum TyReal -> fprintf ppf "real"

let pp_machine ppf m =
  fprintf ppf "machine %s {\n memory: %a\n instances: %a\n reset(){\n%a\n} step(%a){\n%a\n}}\n"
    m.name
    (pp_list ", " (fun ppf (s, Sty ty) -> fprintf ppf "%s:%a" s pp_sty ty)) m.memory
    (pp_list ", " (fun ppf s -> fprintf ppf "%s" s)) m.instances
    pp_ostatement m.reset
    (pp_list ", " (fun ppf (s, ty) -> fprintf ppf "%s" s)) (fst m.step)
    pp_ostatement (snd m.step)

let pp_file = pp_list "\n" pp_machine
