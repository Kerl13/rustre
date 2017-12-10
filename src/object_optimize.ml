open Ast_object


module type OPT = sig
  val apply : file -> file
end


module ConstMatches : OPT = struct
  let rec apply_stmt = function
    | SCase (EConst c, cases) ->
      let dc = begin match c with
        | CBool true -> "True"
        | CBool false -> "False"
        | CDataCons dc -> dc
        | _ -> assert false (* cannot be *)
      end in
      List.assoc dc cases
    | SSeq (s1, s2) -> SSeq (apply_stmt s1, apply_stmt s2)
    | s -> s

  let apply_machine m =
    let reset = apply_stmt m.reset in
    let step =
      let args, temp, out, stmt = m.step in
      args, temp, out, apply_stmt stmt
    in
    { m with step = step ; reset = reset }

  let apply file =
    { file with objf_machines = List.map apply_machine file.objf_machines }
end

let run_all_opts file =
  file
  |> ConstMatches.apply
