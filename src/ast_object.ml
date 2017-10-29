type machine_id = string
type var_id = string
type z = int

type var_list = var_id * Parse_ast.ty list

type machine_ty = Parse_ast.ty list * Parse_ast.ty list
type instance = machine_id
type memory = var_list

type pat = Var of var_id | State of var_id

type _ expr =
  | EVar : pat -> 'a expr
  | EInt: z -> int expr
  | EReal: float -> float expr
  | EBool: bool -> bool expr
  | EAdd: int expr * int expr -> int expr
  | EMinus: int expr * int expr -> int expr
  | EMult: int expr * int expr -> int expr
  | ENot: bool expr -> bool expr


type statement =
  | SAssign of { n:var_id; expr: 'a. 'a expr }
  | SAssignState of { n: var_id; expr: 'a. 'a expr }
  | SSeq of statement * statement
  | SSkip
  | SCall of pat list * machine_id * pat list

type machine = {
  m: memory;
  j: instance list;
  reset: statement;
  step: var_list * statement;
}
