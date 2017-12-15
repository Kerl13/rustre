{
  open Lexing
  open Parser_ext

  exception Error of string

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [
        "and", AND;
        "automaton", AUTOMATON;
        "bool", BOOL;
        "continue", CONTINUE;
        "const", CONST;
        "do", DO;
        "else", ELSE;
        "every", EVERY;
        "end", END;
        "false", CONST_BOOL(false);
        "fby", FBY;
        "if", IF;
        "in", IN;
        "int", INT;
        "last", LAST;
        "match", MATCH;
        "merge", MERGE;
        "mod", MOD;
        "node", NODE;
        "not", NOT;
        "or", OR;
        "pre", PRE;
        "real", REAL;
        "reset", RESET;
        "revery", REVERY;
        "shared", SHARED;
        "state", STATE;
        "then", THEN;
        "true", CONST_BOOL(true);
        "type", TYPE;
        "unless", UNLESS;
        "until", UNTIL;
        "var", VAR;
        "when", WHEN;
        "with", WITH;
      ];
    fun s ->
      try Hashtbl.find h s with Not_found -> IDENT s

  let dc = function
    | "True" -> CONST_BOOL true
    | "False" -> CONST_BOOL false
    | s -> DCIDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let alpha = lowercase | uppercase
let digit = ['0'-'9']
let exponent = ('e' | 'E') ('+' | '-')? digit+
let float = digit+ '.' digit* exponent?
          | digit* '.'digit+ exponent?
          | digit+ exponent
let ident = lowercase (alpha | '_' | digit)*
let dcident = uppercase (alpha | '_' | digit)*
(** Identifiers starting with an uppercase letter are data constructors or automata states *)

rule token = parse
  | '\n'                    { newline lexbuf; token lexbuf }
  | [' ' '\t' '\r']+        { token lexbuf }
  | "--" [^ '\n']* ['\n']   { newline lexbuf; token lexbuf }
  | "/*"                    { comment lexbuf; token lexbuf }
  | ident                   { id_or_keyword (lexeme lexbuf) }
  | dcident                 { dc (lexeme lexbuf) }
  | digit+ as n             { CONST_INT (int_of_string n) }
  | float as f              { CONST_REAL (float_of_string f) }
  | "-"                     { MINUS }
  | "+"                     { PLUS }
  | "*"                     { STAR }
  | "/"                     { SLASH }
  | ">"                     { GT }
  | ">="                    { GE }
  | "<"                     { LT }
  | "<="                    { LE }
  | "<>"                    { NEQ }
  | "=>"                    { IMPL }
  | "->"                    { ARROW }
  | "("                     { LPAR }
  | ")"                     { RPAR }
  | ":"                     { COLON }
  | ";"                     { SEMICOL }
  | "="                     { EQUAL }
  | ","                     { COMMA }
  | "|"                     { PIPE }
  | _ as c                  { raise (Error (Format.sprintf "%c" c)) }
  | eof                     { EOF }

and comment = parse
  | "*/" { () }
  | '\n' { newline lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
