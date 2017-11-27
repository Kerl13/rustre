open Lexing

let usage = Format.sprintf "usage: %s [options] file.lus main" Sys.argv.(0)

let spec = []

let file, main_node =
  let file = ref None in
  let main = ref None in

  let set_file s =
    if not (Filename.check_suffix s ".lus") then
      raise (Arg.Bad "no .lus extension");
    file := Some s
  in
  let set_main s = main := Some s in

  let cpt = ref 0 in
  let set s =
    incr cpt;
    match !cpt with
    | 1 -> set_file s
    | 2 -> set_main s
    | _ -> raise (Arg.Bad "Too many arguments")
  in

  Arg.parse spec set usage;
  (match !file with Some f -> f | None -> Arg.usage spec usage; exit 1),
  (match !main with Some n -> n | None -> Arg.usage spec usage; exit 1)


let report_loc (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  Format.eprintf "File \"%s\", line %d, characters %d-%d:@." file l fc lc


let () =
  let module Clocking = Clocking.W in
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let file = Parser.file Lexer.token lb in
    close_in c;

    Format.printf "=== Parsed file =====\n" ;
    Format.printf "%a\n@." Ast_parsing.pp_file file ;

    Format.printf "Typing… @?";
    let typed = Typing.do_typing file in
    Format.printf "ok\n=== Typed file =====\n";
    Format.printf "%a\n@." Ast_typed.pp_file typed;

    Format.printf "Clocking… @?";
    let clocked = Clocking.clock_file typed in
    Format.printf "ok\n=== Clocks =====\n";
    Format.printf "%a\n@." Ast_clocked.pp_clocks_file clocked;

    Format.printf "Normalization… @?";
    let normalized = Normalization.normalize_file clocked in
    Format.printf "ok@." ;

    Format.printf "Translation into the object language… @?";
    let obc = Object.from_normalized normalized in
    Format.printf "ok\n=== Object =====\n";
    Format.printf "%a\n@." Ast_object.pp_file obc;

    exit 0
  with
  | Lexer.Lexical_error s ->
    report_loc (lexeme_start_p lb, lexeme_end_p lb);
    Format.eprintf "lexical error: %s\n@." s;
    exit 1
  | Parser.Error ->
    report_loc (lexeme_start_p lb, lexeme_end_p lb);
    Format.eprintf "syntax error\n@.";
    exit 1
  | Typing.Expected_type(a, b, loc) ->
    let Typing.(TypedTy a, TypedTy b) = a, b in
    Format.eprintf "got %a expected %a at %@." Ast_typed.pp_ty a Ast_typed.pp_ty b;
    report_loc loc
  | Typing.Expected_num(loc) ->
    Format.eprintf "Expected num at ";
    report_loc loc
