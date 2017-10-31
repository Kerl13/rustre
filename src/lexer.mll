{
  open Lexing
  open Parser

  exception Lexical_error of string

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [
        "and", AND;
        "bool", BOOL;
        "else", ELSE;
        "every", EVERY;
        "false", CONST_BOOL(false);
        "fby", FBY;
        "if", IF;
        "in", IN;
        "int", INT;
        "merge", MERGE;
        "mod", MOD;
        "node", NODE;
        "not", NOT;
        "or", OR;
        "pre", PRE;
        "real", REAL;
        "then", THEN;
        "true", CONST_BOOL(true);
        "var", VAR;
        "when", WHEN;
        "with", WITH;
      ];
    fun s ->
      try Hashtbl.find h s with Not_found -> STRING s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let exponent = ('e' | 'E') ('+' | '-')? digit+
let float = digit+ '.' digit* exponent?
          | digit* '.'digit+ exponent?
          | digit+ exponent
let ident = alpha (alpha | '_' | digit)*

rule token = parse
  | '\n'                    { newline lexbuf; token lexbuf }
  | [' ' '\t' '\r']+        { token lexbuf }
  | "--" [^ '\n']* ['\n']   { newline lexbuf; token lexbuf }
  | "/*"                    { comment lexbuf; token lexbuf }
  | ident                   { id_or_keyword (lexeme lexbuf) }
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
  | _ as c                  { raise (Lexical_error (Format.sprintf "%c" c)) }
  | eof                     { EOF }

and comment = parse
  | "*/" { () }
  | '\n' { newline lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Lexical_error "unterminated comment") }
