open Ast_object


let rec analyze_defs ?(fonct=false) = function
  | SAssign { n; _} -> [n]
  | SSeq(a, b) -> analyze_defs ~fonct a @ analyze_defs ~fonct b
  | SReset(a, b) -> if fonct then [State ("state_" ^ b)] else []
  | SSkip -> []
  | SCall(_, _, i, res) ->
    if fonct then
      res
    else
      (State ("state_" ^ i)) :: res
  | SCase(_, b) ->
    List.map snd b |> List.map (analyze_defs ~fonct) |> List.concat |> List.sort_uniq compare (* XXX alea jacta est *)

let rec expr_has_nil: type a. a oexpr -> bool = function
  | EVar _ -> false
  | EConst (CNil _) -> true
  | EConst (_) -> false
  | EBOp(_, a, b) -> expr_has_nil a || expr_has_nil b
  | EUOp(_, a) -> expr_has_nil a

(* return variables that directly depends on nil values *)
let rec ostatement_get_nils: ostatement -> ident list = function
  | SAssign {n; expr} ->
    if expr_has_nil expr then
      [n]
    else
      []
  | SSeq(a, b) ->
    ostatement_get_nils a @ ostatement_get_nils b
  | SSkip -> []
  | SCall(_) -> []
  | SReset(_) -> []
  | SCase(a, b) ->
    if expr_has_nil a then
      analyze_defs ~fonct:true (SCase (a, b))
    else
      List.map snd b |> List.map ostatement_get_nils |> List.concat |> List.sort_uniq compare

let do_analysis filename node_names =
  List.iter (fun n ->
      let ch = Unix.open_process_in (Format.sprintf "why3 prove %s -T Node%s -G nil_analysis -P Alt-Ergo" filename n)
      in
        let ok = ref true in
      begin
        try
          while true do
            let s = input_line ch in
            try
            let i = String.index s ':' in
            if String.contains_from s i 'U' then
              ok := false
            else if not (String.contains_from s i 'V') then
              raise Not_found
            with
            | Not_found -> Format.printf "Invalid why3 output: @\n";
              print_endline s;
          done;
        with
        | End_of_file -> ()
      end;
     let stat = Unix.close_process_in ch in

     match stat with
     | Unix.WEXITED(0) ->
       if !ok then
         Format.printf "Nil OK@\n"
       else
         Format.printf "Nil NOT OK@\n"
     | _ -> Format.printf "Nil checking failed@\n"
    ) node_names;
