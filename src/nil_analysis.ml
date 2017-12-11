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
