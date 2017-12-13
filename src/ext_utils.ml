open Ast_ext

  let dummy_loc e pos = { expr_desc = e ; expr_loc = (pos, pos) }
  let zero = dummy_loc (EConst (CInt 0)) Lexing.dummy_pos

  (** Syntactic sugar *)

  let mk_false_at pos = dummy_loc (EConst (CBool false)) pos

  (* f(x0, x1, …)  ~>  f(x1, x2, …) every False *)
  let application f args every endpos = match every with
    | Some e -> EApp (f, args, e)
    | None -> EApp (f, args, mk_false_at endpos)

  (* if x then e1 else e2  ~>  merge x (True  -> e1 when True(x))
                                       (False -> e2 when False(x)) *)
  let if_then_else x e1 e2 endpos =
    EMerge (x, [
      "True",  dummy_loc (EWhen (e1, "True", x)) endpos ;
      "False", dummy_loc (EWhen (e2, "False", x)) endpos
    ])

  (* e1 -> e2  ~>  if (True fby False) then e1 else e2 *)
  let arrow e1 e2 endpos =
    (* since merge's first argument must be an identifier, we use "_TfbyF" to
     * refer an previously defined variable equal to [true fby false]. This
     * variable is automatically added by the parser. *)
    if_then_else "_TfbyF" e1 e2 endpos

  (* pre e  ~>  nil fby e *)
  let pre e = EFby (CNil, e)
let if_then_else x e1 e2 endpos =
  EMerge (x, [
      "True",  dummy_loc (EWhen (e1, "True", x)) endpos ;
      "False", dummy_loc (EWhen (e2, "False", x)) endpos
    ])

