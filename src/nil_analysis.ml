open Ast_object

let rec filter_map: ('a -> 'b option) -> 'a list -> 'b list = fun f l ->
  match l with
  | [] -> []
  | t::q -> match f t with
    | Some s -> s :: filter_map f q
    | None -> filter_map f q

let rec analyze_defs ?(fonct=false) = function
  | SAssign { n; _} -> [n]
  | SSeq(a, b) -> analyze_defs ~fonct a @ analyze_defs ~fonct b
  | SReset(_, b) -> if fonct then [State b] else []
  | SSkip -> []
  | SCall(_, _, _, res) ->
    res
  | SCase(_, b) ->
    List.map snd b |> List.map (analyze_defs ~fonct) |> List.concat |> List.sort_uniq compare 

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

type res = Success | Fail | Invalid

let try_nil filename n inline =
  let ch =
    if inline then
      Unix.open_process_in (Format.sprintf "why3 prove %s -T Node%s -t 2 -G nil_analysis -a split_goal_wp -a inline_all -P Z3" filename n)
    else
      Unix.open_process_in (Format.sprintf "why3 prove %s -T Node%s -t 2 -G nil_analysis -P Z3" filename n)
  in
  let ok = ref true in
  begin
    try
      while true do
        let s = input_line ch in
        try
          let i = String.index s ':' in
          if String.contains_from s i 'U' || String.contains_from s i 'T' then
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
      Success
    else
      Invalid
  | _ -> Fail

let do_analysis filename node_names =
  List.iter (fun n ->
      let res = try_nil filename n false in

      match res with
      | Fail ->
        Format.printf "Nil checking failed@\n"
      | Success ->
        Format.printf "%s: Nil \027[32mOK\027[0m@." n
      | Invalid ->
        match try_nil filename n true with
        | Fail ->
          Format.printf "Nil checking failed@\n"
        | Success ->
          Format.printf "%s: Nil \027[32mOK\027[0m@." n
        | Invalid ->
          Format.printf "%s: Nil \027[31mNOT OK\027[0m@." n
    ) node_names;
