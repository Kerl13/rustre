%{
  open Ast_parsing


  let dummy_loc e pos = { expr_desc = e ; expr_loc = (pos, pos) }
  let pat_descs patterns = List.map (fun p -> p.pat_desc) patterns

  (** Syntactic sugar *)

  let mk_false_at pos = dummy_loc (EConst (CBool false)) pos

  (* f(x0, x1, …)  ~>  f(x1, x2, …) every False *)
  let application f args every endpos = match every with
    | Some e -> EApp (f, args, e)
    | None -> EApp (f, args, mk_false_at endpos)

  (* if x then e1 else e2  ~>  merge x (True  -> e1 when True(x))
                                       (False -> e2 when False(x)) *)
  let if_then_else x e1 e2 endpos =
    EMerge (x, [
      "True",  dummy_loc (EWhen (e1, "True", x)) endpos ;
      "False", dummy_loc (EWhen (e2, "False", x)) endpos
    ])

  (* e1 -> e2  ~>  if (True fby False) then e1 else e2 *)
  let arrow _ _ = failwith "Not implemented"

  (* pre e  ~>  nil fby e *)
  let pre e = EFby (CNil, e)
%}


%token <string> IDENT DCIDENT
%token <bool> CONST_BOOL
%token <int> CONST_INT
%token <float> CONST_REAL

%token BOOL INT REAL TYPE

%token PLUS MINUS STAR SLASH MOD
%token LT LE GT GE
%token EQUAL NEQ
%token AND OR IMPL NOT
%token ARROW PRE FBY
%token IF THEN ELSE
%token WHEN EVERY MERGE

%token NODE VAR WITH IN

%token COMMA COLON SEMICOL
%token LPAR RPAR
%token EOF


%left ELSE
%right ARROW
%left IMPL
%left OR
%left AND
%left EQUAL NEQ LT LE GT GE
%right PLUS MINUS
%right WHEN
%left STAR SLASH MOD
%right EVERY
%right NOT
%right FBY
%right PRE


%start file
%type <Ast_parsing.file> file

%%


%inline located(E): (* located expression *)
| e = E  {{ expr_desc = e ; expr_loc = ($startpos, $endpos) }}

%inline clist(X): (* comma-separated list *)
| xs = separated_list(COMMA, X)  { xs }

%inline nclist(X): (* non empty comma-separated list *)
| xs = separated_nonempty_list(COMMA, X)  { xs }

%inline nclist2(X): (* comma-separated list of length ≥ 2 *)
| x = X COMMA xs = separated_nonempty_list(COMMA, X)  { x :: xs }


ident:    s = IDENT    { s }
dcident : s = DCIDENT  { s }
dcident_or_boolean:
| dc = dcident         { dc }
| b = CONST_BOOL       { if b then "True" else "False" }


typ:
| BOOL   { TyBool }
| INT    { TyInt }
| REAL   { TyReal }


file:
  typedefs = list(typedef) nodes = list(node) EOF
  {{ f_typedefs = typedefs; f_nodes = nodes }}


typedef:
  TYPE t = ident EQUAL dcs = separated_nonempty_list(PLUS, dcident)
  { t, dcs }


node:
  NODE name = ident LPAR in_params = param_list RPAR
  EQUAL LPAR out_params = param_list RPAR
  WITH local_params = loption(delimited(VAR, param_list, IN))
  eqs = separated_nonempty_list(SEMICOL, equation)
  {{
    n_name = name ;
    n_input = in_params ;
    n_output = out_params ;
    n_local = local_params ;
    n_eqs = eqs ;
    n_loc = ($startpos, $endpos) ;
  }}


param_list:
  ps = separated_nonempty_list(SEMICOL, param)
  { List.flatten ps }


param:
  ids = nclist(ident) COLON t = typ
  { List.map (fun id -> (id, t)) ids }


equation:
  p = pattern EQUAL e = located(expr)
  {{ eq_pat = p ; eq_expr = e }}


pattern:
| x = ident
    {{ pat_desc = PIdent x ; pat_loc = ($startpos, $endpos) }}
| LPAR pats = nclist(pattern) RPAR
    {{ pat_desc = PTuple (pat_descs pats) ; pat_loc = ($startpos, $endpos) }}


expr:
| LPAR e = expr RPAR                           { e }
| c = const                                    { EConst c }
| x = ident                                    { EIdent x }
| LPAR es = nclist2(located(expr)) RPAR        { ETuple es }
| v = const FBY e = located(expr)              { EFby (v, e) }

| IF x = ident
  THEN ethen = located(expr)
  ELSE eelse = located(expr)                   { if_then_else x ethen eelse $endpos }

| e1 = located(expr) PLUS  e2 = located(expr)  { EOp (OpAdd, [e1 ; e2]) }
| e1 = located(expr) MINUS e2 = located(expr)  { EOp (OpSub, [e1 ; e2]) }
| e1 = located(expr) STAR  e2 = located(expr)  { EOp (OpMul, [e1 ; e2]) }
| e1 = located(expr) SLASH e2 = located(expr)  { EOp (OpDiv, [e1 ; e2]) }
| e1 = located(expr) MOD   e2 = located(expr)  { EOp (OpMod, [e1 ; e2]) }
| e1 = located(expr) LT    e2 = located(expr)  { EOp (OpLt, [e1 ; e2]) }
| e1 = located(expr) LE    e2 = located(expr)  { EOp (OpLe, [e1 ; e2]) }
| e1 = located(expr) GT    e2 = located(expr)  { EOp (OpGt, [e1 ; e2]) }
| e1 = located(expr) GE    e2 = located(expr)  { EOp (OpGe, [e1 ; e2]) }
| e1 = located(expr) EQUAL e2 = located(expr)  { EOp (OpEq, [e1 ; e2]) }
| e1 = located(expr) NEQ   e2 = located(expr)  { EOp (OpNeq, [e1 ; e2]) }
| e1 = located(expr) AND   e2 = located(expr)  { EOp (OpAnd, [e1 ; e2]) }
| e1 = located(expr) OR    e2 = located(expr)  { EOp (OpOr, [e1 ; e2]) }
| e1 = located(expr) IMPL  e2 = located(expr)  { EOp (OpImpl, [e1 ; e2]) }
| NOT e = located(expr)                        { EOp (OpNot, [e]) }
| MINUS e = located(expr)                      { EOp (OpSub, [e]) }
| PRE e = located(expr)                        { pre e }
| e1 = located(expr) ARROW e2 = located(expr)  { arrow e1 e2 }

| f = ident
  LPAR args = clist(located(expr)) RPAR
  ev = option(preceded(EVERY, located(expr)))  { application f args ev $endpos }

| e = located(expr)
  WHEN dc = dcident_or_boolean
  LPAR x = ident RPAR                          { EWhen (e, dc, x) }
| MERGE x = ident
  clauses = nonempty_list(merge_clause)        { EMerge (x, clauses) }


merge_clause:
  LPAR dc = dcident_or_boolean ARROW e = located(expr) RPAR
  { (dc, e) }


const:
| b = CONST_BOOL  { CBool b }
| n = CONST_INT   { CInt n }
| f = CONST_REAL  { CReal f }
