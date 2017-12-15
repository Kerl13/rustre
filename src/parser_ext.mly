%{
  open Ast_ext
  open Ext_utils


  let pat_descs patterns = List.map (fun p -> p.pat_desc) patterns

  (** Global constants *)
  let consts = Hashtbl.create 17

  (* Type definitions *)
  let data_constructors = Hashtbl.create 17


  let mk_local ids init = match init with
    | None -> List.map (fun id -> (id, None)) ids
    | Some l -> List.map2 (fun id ini -> (id, Some ini) ) ids l

  (** Define _TfbyF when it is required *)

  (* _TfbyF = true fby false *)
  let tfbyf =
    let pos = Lexing.dummy_pos in
    let e = EFby (CBool true, mk_false_at pos) in
    let e = dummy_loc e pos in
    let pat = { pat_desc = PIdent "_TfbyF" ; pat_loc = (pos, pos) } in
    { eq_desc = EEq (pat, e) ; eq_loc = (pos, pos)}

  (* add [_TfbyF = true fby false] to the node's equations if required *)
  let add_tfbyf node =
    let find s = function
      | EMerge ("_TfbyF", _) -> true
      | _ -> s
    in
    let present =
      let find_in_node found eq = visit_eq find found eq in
      List.fold_left find_in_node false node.n_eqs
    in
    if present then
      let node = { node with n_eqs = tfbyf :: node.n_eqs } in
      let pos = Lexing.dummy_pos in
      { node with n_local = {v_name = "_TfbyF" ; v_type = TyBool ; v_shared = false ; v_init = None ; v_loc = (pos, pos)} :: node.n_local }
    else
      node
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

%token CONST
%token NODE VAR WITH IN

%token COMMA COLON SEMICOL
%token LPAR RPAR
%token EOF
%token PIPE

%token AUTOMATON STATE DO END
%token UNTIL UNLESS CONTINUE
%token RESET REVERY MATCH
%token SHARED LAST


%left ELSE
%right ARROW
%right EVERY
%left IMPL
%left OR
%left AND
%left EQUAL NEQ LT LE GT GE
%right WHEN
%right PLUS MINUS
%left STAR SLASH MOD
%right NOT
%right FBY
%right PRE


%start file
%type <Ast_ext.file> file

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
| BOOL       { TyBool }
| INT        { TyInt }
| REAL       { TyReal }
| t = ident  { TyEnum (t, Hashtbl.find data_constructors t) }


file:
  typedefs = list(typedef) list(global_const) nodes = list(node) EOF
  {{ f_typedefs = typedefs; f_nodes = nodes }}


typedef:
  TYPE t = ident EQUAL dcs = separated_nonempty_list(PLUS, dcident)
  { Hashtbl.add data_constructors t dcs ; (t, dcs) }


global_const:
  CONST name = DCIDENT EQUAL c = const
  { Hashtbl.add consts name c }


node:
  NODE name = ident LPAR in_params = param_list RPAR
  EQUAL LPAR out_params = param_list RPAR
  WITH local_params = loption(delimited(VAR, local_list, IN))
  eqs = separated_nonempty_list(SEMICOL, equation)
  {
    let node = {
      n_name = name ;
      n_input = in_params ;
      n_output = out_params ;
      n_local = local_params ;
      n_eqs = eqs ;
      n_loc = ($startpos, $endpos) ;
    } in
    add_tfbyf node
  }


param_list:
  ps = separated_list(SEMICOL, param)
  { List.flatten ps }

param:
  ids = nclist(ident) COLON t = typ
  { List.map (fun id -> (id, t)) ids }

local_list:
  ps = separated_nonempty_list(SEMICOL, local)
  { List.flatten ps }

local:
  sh = boption(SHARED) ids = nclist(ident) COLON t = typ
  i = option(preceded(EQUAL,nclist(const)))
  { let li = mk_local ids i in
   List.map (fun (id, ini) ->
   {v_name = id ; v_type = t ; v_shared = sh ;
    v_init = ini ; v_loc = ($startpos, $endpos)} ) li
  }

equation:
| p = pattern EQUAL e = located(expr)
  {{ eq_desc = EEq (p, e) ; eq_loc = ($startpos, $endpos) }}
| MATCH x = ident WITH PIPE? hl = separated_nonempty_list(PIPE, match_handler) END
  {{ eq_desc = EMatch (x, hl); eq_loc = ($startpos, $endpos) }}
| RESET eqs = separated_nonempty_list(SEMICOL, equation) REVERY e = located(expr)
  {{ eq_desc = EReset (eqs, e) ; eq_loc = ($startpos, $endpos) }}
| AUTOMATON states = nonempty_list(state_handler) END
  {{ eq_desc = EAutomaton states; eq_loc = ($startpos, $endpos) }}

pattern:
| x = ident
    {{ pat_desc = PIdent x ; pat_loc = ($startpos, $endpos) }}
| LPAR pats = nclist(pattern) RPAR
    {{ pat_desc = PTuple (pat_descs pats) ; pat_loc = ($startpos, $endpos) }}

match_handler:
  c = dcident DO eqs = separated_nonempty_list(SEMICOL, equation)
  {{ m_name = c ; m_eqs = eqs }}

state_handler:
  STATE s = dcident
  loc_decl = loption(delimited(VAR, local_list, IN))
  DO eqs = separated_list(SEMICOL, equation)
  ut = list(until_escape)
  ul = list(unless_escape)
  {{ s_name = s ; s_local = loc_decl ; s_eqs = eqs ; s_until = ut ; s_unless = ul }}

until_escape:
  UNTIL e = escape
  {e}

unless_escape:
  UNLESS e = escape
  {e}

escape:
| e = located(expr) THEN s = dcident
  {{e_cond = e ; e_reset = true ; e_next = s}}
| e = located(expr) CONTINUE s = dcident
  {{e_cond = e ; e_reset = false ; e_next = s}}

expr:
| LPAR e = expr RPAR                           { e }
| c = const                                    { EConst c }
| x = ident                                    { EIdent x }
| LAST x = ident                               { ELast x }
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
| MINUS e = located(expr)                      { EOp (OpSub, [zero; e]) }
| PRE e = located(expr)                        { pre e }
| e1 = located(expr) ARROW e2 = located(expr)  { arrow e1 e2 $endpos }

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
| dc = dcident    { if Hashtbl.mem consts dc
                    then Hashtbl.find consts dc
                    else CDataCons dc }
