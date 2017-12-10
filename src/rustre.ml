open Lexing

let usage = Format.sprintf "usage: %s [options] file.lus main" Sys.argv.(0)


type extractor = Rust | Why3

let file, main_node, extractor, output, verbose =
  let extractor = ref Rust in
  let output = ref "" in
  let verbose = ref false in
  let spec = ["-extract",
              Arg.Symbol (["why3"; "rust"], (fun s ->
                  extractor := if s = "why3" then Why3 else Rust)),
              "Extract to why3 or rust";
              "-o", Arg.Set_string output, "File to write the generated code.";
              "-v", Arg.Set verbose, "Verbose output"] in
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
  (match !main with Some n -> n | None -> Arg.usage spec usage; exit 1),
  !extractor,
  !output,
  !verbose

module Extractor = (val (match extractor with
    | Rust -> (module Extract_rust.E)
    | Why3 -> (module Extract_why3.E)): Extract.E)


let report_loc (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  Format.eprintf "File \"%s\", line %d, characters %d-%d:@." file l fc lc


let () =
  let module Clocking = Clocking.W in
  let module Scheduling = Scheduling.Simple in
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let file = Parser.file Lexer.token lb in
    close_in c;
    let empty_formatter = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ()) in
    let format = if verbose then Format.std_formatter else empty_formatter in

    Format.fprintf format "=== Parsed file =====\n" ;
    Format.fprintf format "%a\n@." Ast_parsing.pp_file file ;

    Format.fprintf format "Typing… @?";
    let typed = Typing.do_typing file in
    Format.fprintf format "ok\n=== Typed file =====\n";
    Format.fprintf format "%a\n@." Ast_typed.pp_file typed;

    Format.fprintf format "Clocking… @?";
    let clocked = Clocking.clock_file typed main_node in
    Format.fprintf format "ok\n=== Clocks =====\n";
    Format.fprintf format "%a\n@." Ast_clocked.pp_clocks_file clocked;

    Format.fprintf format "Normalization… @?";
    let normalized = Normalization.normalize_file clocked in
    Format.fprintf format "ok@." ;

    Format.fprintf format "Scheduling… @?";
    let scheduled = Scheduling.schedule normalized main_node in
    Format.fprintf format "ok\n=== Scheduled nodes =====\n";
    Format.fprintf format "%a\n@." Ast_normalized.pp_file scheduled;

    Format.fprintf format "Translation into the object language… @?";
    let obc = Object.from_normalized scheduled in
    Format.fprintf format "ok\n=== Object =====\n";
    Format.fprintf format "%a\n@." Ast_object.pp_file obc;

    Format.printf "Extracting…@\n";
    let locs = List.map (fun n ->
        n.Ast_parsing.n_name, n.Ast_parsing.n_local
        |> List.map (fun (i, ty) ->
            i, let Typing.TypedTy ty = Typing.ty_to_typed_ty ty in
            Ast_object.Sty ty)
      ) file.Ast_parsing.f_nodes in
    if output = "" then
      Format.printf "%a@." Extractor.extract_to (obc, main_node, locs)
    else Extractor.extract_to (Format.formatter_of_out_channel (open_out output)) (obc, main_node, locs);

    (* let states = Ast_object.extract_states obc in

    Format.printf "Spec:@\n%a@." Specifications.spec_file (states, file);
       Format.fprintf (Format.formatter_of_out_channel (open_out "spec.mlw")) "%a@." Specifications.spec_file (states, file); *)


    exit 0
    with
  | Lexer.Error s ->
    report_loc (lexeme_start_p lb, lexeme_end_p lb);
    Format.eprintf "lexical error: %s\n@." s;
    exit 1
  | Parser.Error ->
    report_loc (lexeme_start_p lb, lexeme_end_p lb);
    Format.eprintf "syntax error\n@.";
    exit 1
  | Typing.Error (loc, message) ->
    report_loc loc;
    Format.eprintf "%s@." message ;
    exit 1
  | Clocking.Error (loc, message) ->
    report_loc loc;
    Format.eprintf "%s@." message;
    exit 1
  | Scheduling.Error message ->
    Format.eprintf "%s@." message;
    exit 1
