type machine_id = string
type var_id = string
type z = int

type var_list = var_id * Ast_parsing.ty list

type machine_ty = Ast_parsing.ty list * Ast_parsing.ty list
type instance = machine_id
type memory = var_list

type ident = Var of var_id | State of var_id

type _ expr =
  | EVar : ident -> 'a expr
  | EInt: z -> int expr
  | EReal: float -> float expr
  | EBool: bool -> bool expr
  | EAdd: int expr * int expr -> int expr
  | EMinus: int expr * int expr -> int expr
  | EMult: int expr * int expr -> int expr
  | ENot: bool expr -> bool expr

type statement =
  | SAssign of { n: ident; expr: 'a. 'a expr }
  | SSeq of statement * statement
  | SSkip
  | SCall of ident list * machine_id * ident list
  | SReset of machine_id
  | SCase of ident * (statement list) (* Constructors are numbered, the nth
                                       statement corresponds to the nth
                                       constructor -- 2 in case of Booleans *)

type machine = {
  m: memory;
  j: instance list;
  reset: statement;
  step: var_list * statement;
}
