open Lexing

(* XXX: To remove, that was only to force compilation of these modules. *)
open Ast_typed_utils
open Clocking
   
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
  Format.eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc


let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let file = Parser.file Lexer.token lb in
    close_in c;

    Format.printf "=== Parsed file =====\n%a@." Ast_parsing.pp_file file;
    Format.printf "Typing…@.";
    Typing.do_typing file |> Format.printf "%a@.ok@." Ast_typed.pp_file;

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
    Format.eprintf "got %a expected %a@." Ast_typed.pp_ty a Ast_typed.pp_ty b;
  | e ->
    Format.eprintf "Anomaly: %s\n@." (Printexc.to_string e);
    exit 2
