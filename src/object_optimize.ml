open Ast_object


module type OPT = sig
  val apply : file -> file
end


module ConstMatches : OPT = struct
  let rec simplify_oexpr : type a. a oexpr -> a oexpr = fun e ->
    match e with
    | EBOp (Ast_typed.OpAnd, (EConst (CBool true)), e') -> simplify_oexpr e'
    | EBOp (Ast_typed.OpAnd, e', (EConst (CBool true))) -> simplify_oexpr e'
    | EBOp (Ast_typed.OpAnd, _, (EConst (CBool false))) -> EConst (CBool false)
    | EBOp (Ast_typed.OpAnd, (EConst (CBool false)), _) -> EConst (CBool false)
    | _ -> e

  let rec apply_stmt = function
    | SCase (e, cases) ->
       let cases = List.map (fun (s, stmt) -> (s, apply_stmt stmt)) cases in
       let e' = simplify_oexpr e in
       begin match e' with
       | EConst c ->
          begin
            let dc = begin match c with
                     | CBool true -> "True"
                     | CBool false -> "False"
                     | CDataCons dc -> dc
                     | _ -> assert false (* cannot be *)
                     end in
            List.assoc dc cases
          end
       | _ -> SCase (e', cases) end
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


module ConsecutiveMatches : OPT = struct
  let const_eq : type a b. a oconst -> b oconst -> bool
    = fun c1 c2 -> match (c1, c2) with
      | CBool b1, CBool b2 -> b1 = b2
      | CInt n1, CInt n2 -> n1 = n2
      | CReal r1, CReal r2 -> r1 = r2
      | CDataCons dc1, CDataCons dc2 -> dc1 = dc2
      | _ -> false

  let bop_eq : type a b c d. (a, b) Ast_typed.binop -> (c, d) Ast_typed.binop -> bool
    = fun bop1 bop2 -> match (bop1, bop2) with
      | Ast_typed.OpAdd,  Ast_typed.OpAdd  -> true
      | Ast_typed.OpSub,  Ast_typed.OpSub  -> true
      | Ast_typed.OpMul,  Ast_typed.OpMul  -> true
      | Ast_typed.OpDiv,  Ast_typed.OpDiv  -> true
      | Ast_typed.OpMod,  Ast_typed.OpMod  -> true
      | Ast_typed.OpLt,   Ast_typed.OpLt   -> true
      | Ast_typed.OpLe,   Ast_typed.OpLe   -> true
      | Ast_typed.OpGt,   Ast_typed.OpGt   -> true
      | Ast_typed.OpGe,   Ast_typed.OpGe   -> true
      | Ast_typed.OpEq,   Ast_typed.OpEq   -> true
      | Ast_typed.OpNeq,  Ast_typed.OpNeq  -> true
      | Ast_typed.OpAnd,  Ast_typed.OpAnd  -> true
      | Ast_typed.OpOr,   Ast_typed.OpOr   -> true
      | Ast_typed.OpImpl, Ast_typed.OpImpl -> true
      | _ -> false

  let uop_eq : type a b c d. (a, b) Ast_typed.unop -> (c, d) Ast_typed.unop -> bool
    = fun uop1 uop2 -> match (uop1, uop2) with
      | Ast_typed.OpNot, Ast_typed.OpNot -> true
      | Ast_typed.OpUMinus, Ast_typed.OpUMinus -> true
      | _ -> false

  let rec expr_eq : type a b. a oexpr -> b oexpr -> bool
    = fun e1 e2 -> match (e1, e2) with
      | EVar x, EVar y -> x = y
      | EConst c1, EConst c2 -> const_eq c1 c2
      | EBOp (b1, e11, e12), EBOp (b2, e21, e22) -> bop_eq b1 b2 && expr_eq e11 e21 && expr_eq e12 e22
      | EUOp (op1, e1), EUOp (op2, e2) -> uop_eq op1 op2 && expr_eq e1 e2
      | _ -> false

  let fuse_cases cases1 cases2 =
      let cases1 = List.sort (fun (dc1, _) (dc2, _) -> compare dc1 dc2) cases1 in
      let cases2 = List.sort (fun (dc1, _) (dc2, _) -> compare dc1 dc2) cases2 in
      let zip (dc1, case1) (dc2, case2) =
        assert (dc1 = dc2) ; (* Sanity check *)
        (dc1, SSeq (case1, case2))
      in
      List.map2 zip cases1 cases2

  let rec cut_last = function
    | SSeq (s1, SSeq (s2, s3)) -> cut_last (SSeq (SSeq (s1, s2), s3))
    | SSeq (s1, s2) -> s1, s2
    | s -> SSkip, s

  let rec cut_fst = function
    | SSeq (SSeq (s1, s2), s3) -> cut_fst (SSeq (s1, SSeq (s2, s3)))
    | SSeq (s1, s2) -> s1, s2
    | s -> s, SSkip

  let rec apply_stmt = function
    | SSeq (s1, s2) ->
      let s1', last = cut_last s1 in  (* s1 = ... ; last *)
      let fst, s2' = cut_fst s2 in    (* s2 = fst ; ...  *)
      begin match (last, fst) with
        | SCase (e1, cases1), SCase (e2, cases2) when expr_eq e1 e2 ->
          (* fuse last and fst if they are matchings on the same expressions *)
          let cases = fuse_cases cases1 cases2 in
          apply_stmt @@ SSeq (s1', SSeq (SCase (e1, cases), s2'))
        | _ -> SSeq (apply_stmt s1, apply_stmt s2)
      end
    | SCase (e, cases) -> SCase (e, List.map (fun (dc, stmt) -> dc, apply_stmt stmt) cases)
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
  |> ConsecutiveMatches.apply
