open Lexing

let usage = Format.sprintf "usage: %s [options] file.lus main" Sys.argv.(0)


type extractor = Rust | Why3

let file, main_node, extractor, output, verbose, opt, specification, nils, ext, verif =
  let extractor = ref Rust in
  let output = ref "" in
  let verbose = ref false in
  let opt = ref false in
  let specification = ref false in
  let verif = ref false in
  let nils = ref false in
  let ext = ref false in
  let spec = [
    "-extract",
    Arg.Symbol (["why3"; "rust"],
                (fun s -> extractor := if s = "why3" then Why3 else Rust)),
    "Extract to why3 or rust";
    "-o", Arg.Set_string output, "File to write the generated code.";
    "-v", Arg.Set verbose, "Verbose output";
    "-opt", Arg.Set opt, "Optimize object code";
    "-spec", Arg.Set specification, "Prove compilation";
    "-verif", Arg.Set verif, "Do verification";
    "-nils", Arg.Set nils, "Prove that nils are not used";
    "-ext", Arg.Set ext, "Use automata extension"
  ] in
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
  !verbose,
  !opt,
  !specification,
  !nils,
  !ext,
  !verif

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
  let module Checkclocking = Checkclocking.CheckW in
  let module Scheduling = Scheduling.Simple in
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let empty_formatter = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ()) in
    let format = if verbose then Format.std_formatter else empty_formatter in


    let file =
      if ext then begin
        let file = Parser_ext.file Lexer_ext.token lb in
        close_in c;
        Format.fprintf format "=== Parsed file =====\n" ;
        Format.fprintf format "%a\n@." Ast_ext.pp_file file ;

        let file = Match.tr_file file in

        let file = Reset.tr_file file in

        let trans = Ext_to_base.tr_file file in
        Format.fprintf format "=== Translated file =====\n" ;
        Format.fprintf format "%a\n@." Ast_parsing.pp_file trans ;
        trans
      end

      else begin
        let file = Parser.file Lexer.token lb in
        close_in c;

        Format.fprintf format "=== Parsed file =====\n" ;
        Format.fprintf format "%a\n@." Ast_parsing.pp_file file ;
        file
      end in


    Format.fprintf format "Typing… @?";
    let typed = Typing.do_typing file in
    Format.fprintf format "ok\n=== Typed file =====\n";
    Format.fprintf format "%a\n@." Ast_typed.pp_file typed;

    Format.fprintf format "Clocking… @?";
    let clocked = Clocking.clock_file typed main_node in
    Format.fprintf format "ok\n=== Clocks =====\n";
    Format.fprintf format "%a\n@." Ast_clocked.pp_clocks_file clocked;
    Checkclocking.check_clock_file typed main_node clocked;

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

    let obc = if opt then begin
        Format.fprintf format "Optimizing object code… @?";
        let obc = Object_optimize.run_all_opts obc in
        Format.fprintf format "ok\n=== Object (optimized) =====\n";
        Format.fprintf format "%a\n@." Ast_object.pp_file obc;
        obc
      end else obc in

    Format.printf "Extracting…@\n";
    let locs = List.map (fun n ->
        n.Ast_parsing.n_name, n.Ast_parsing.n_local
                              |> List.map (fun (i, ty) ->
                                  i, let Typing.TypedTy ty = Typing.ty_to_typed_ty ty in
                                  Ast_object.Sty ty)
      ) file.Ast_parsing.f_nodes in
    if output = "" then
      Format.printf "%a@." Extractor.extract_to (obc, main_node, locs)
    else
      begin
        let out_ch = open_out output in
        Extractor.extract_to (Format.formatter_of_out_channel out_ch) (obc, main_node, locs);
        close_out out_ch
      end;

    if nils then
      begin
        if output = "" || extractor <> Why3 then
          (Format.printf "Error: must extract to Why3 and write to a file to check nils (options -extract why3 -o <file>.mlw)"; exit 1);
        Nil_analysis.do_analysis output (List.map (fun n -> n.Ast_parsing.n_name) file.Ast_parsing.f_nodes)
      end;

    if verif then
      begin
        if output = "" || extractor <> Why3 then
          (Format.printf "Error: must extract to Why3 and write to a file to do verification (options -extract why3 -o <file>.mlw)"; exit 1);

        Verification.do_init output (List.map (fun n -> n.Ast_parsing.n_name) file.Ast_parsing.f_nodes)
      end;


    if specification then
      begin
        let states = Ast_object.extract_states obc in

        Format.printf "Spec:@\n%a@." Specifications.spec_file (states, file);
        Format.fprintf (Format.formatter_of_out_channel (open_out "spec.mlw")) "%a@." Specifications.spec_file (states, file);
        try
          Format.printf "Coq proof:@\n%a@." Semantic_proof.spec_file (states, file, obc);
        with
        | Sys_error _ -> Format.printf "Session Why3 non créée, voir le README@."
      end;


    exit 0
  with
  | Lexer.Error s
  | Lexer_ext.Error s ->
    report_loc (lexeme_start_p lb, lexeme_end_p lb);
    Format.eprintf "lexical error: %s\n@." s;
    exit 1
  | Ast_parsing.MError s ->
    report_loc (lexeme_start_p lb, lexeme_end_p lb);
    Format.eprintf "syntax error: %s\n@." s;
    exit 1
  | Parser.Error
  | Parser_ext.Error ->
    report_loc (lexeme_start_p lb, lexeme_end_p lb);
    Format.eprintf "syntax error\n@.";
    exit 1
  | Match.Error (loc, message) ->
    report_loc loc;
    Format.eprintf "%s@." message ;
    exit 1
  | Typing.Error (loc, message) ->
    report_loc loc;
    Format.eprintf "%s@." message ;
    exit 1
  | Clocking.Error (loc, message) ->
    report_loc loc;
    Format.eprintf "%s@." message;
    exit 1
  | Scheduling.Error (loc, message) ->
    report_loc loc;
    Format.eprintf "%s@." message;
    exit 1
  | Checkclocking.Error (loc, message) ->
    report_loc loc;
    Format.eprintf "Clocking internal error: %s@." message;
    exit 1
