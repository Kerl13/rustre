let sty_for_const: type a. a Ast_typed.const -> a Ast_normalized.sty = fun a ->
  match a with
  | Ast_typed.CNil -> assert false
  | Ast_typed.CBool _ -> Ast_normalized.StyBool
  | Ast_typed.CInt _ -> Ast_normalized.StyNum Ast_typed.TyZ
  | Ast_typed.CReal _ ->  Ast_normalized.StyNum Ast_typed.TyReal

let sty_for_ty: type a. a Ast_typed.ty -> a Ast_normalized.sty = fun t ->
  match t with
  | Ast_typed.TyBool -> Ast_normalized.StyBool
  | Ast_typed.TyNum a -> Ast_normalized.StyNum a
  | Ast_typed.TyPair (_,_) -> assert false

let new_var_ =
  let counter = ref 0 in
  fun () ->
    incr counter; Format.sprintf "var%d" !counter

let new_var (Ast_clocked.NodeLocal b) ty =
  let id = new_var_ () in
  id, Ast_clocked.NodeLocal (Ast_typed.VTuple (id, ty, b))

let rec come_up_vars: type a. 'b -> a Ast_typed.ty -> a Ast_typed.var_list * 'b =
  fun b ty ->
    match ty with
    | Ast_typed.TyBool ->
      let a, b = new_var b ty in
      Ast_typed.VIdent(a, ty), b
    | Ast_typed.TyNum _ ->
      let a, b = new_var b ty in
      Ast_typed.VIdent(a, ty), b
    | Ast_typed.TyPair(t1, t2) ->
      let a, b = new_var b t1 in
      let vl, b = come_up_vars b t2 in
      Ast_typed.VTuple(a, t1, vl), b

let mono_ident Ast_clocked.{ pat_desc; } =
  match pat_desc with
  | Ast_typed.VIdent(v, _) -> v
  | _ -> assert false


let rec normalize_eqs (a, b) (Ast_clocked.Equ (pat, expr)) =
  let a, b, _ = normalize_expr a b (Some pat) expr in a, b

(* So the main problem with this function is that we cannot always return a
   `a nexpr`, sometimes that type is not inhabitated. Let's return Cnil instead ?? *)
and normalize_expr: type a. Ast_normalized.nequation list -> Ast_clocked.node_local -> a Ast_clocked.pattern option -> a Ast_clocked.cexpr ->
  Ast_normalized.nequation list * Ast_clocked.node_local * a Ast_normalized.nexpr = fun a b pat
  Ast_clocked.{ texpr_desc; texpr_type; texpr_clock; texpr_loc } ->
  let open Ast_normalized in
  let open Ast_clocked in
  match texpr_desc with
  | CConst c ->
    let sty = sty_for_const c in
    let nexpr = {
      nexpr_desc = NConst c;
      nexpr_type = sty;
      nexpr_clock = texpr_clock;
      nexpr_loc = texpr_loc;
    } in
    let nexpr_merge = {
      nexpr_merge_desc = NExpr nexpr;
      nexpr_merge_type = sty;
      nexpr_merge_clock = texpr_clock;
      nexpr_merge_loc = texpr_loc;
    } in
    begin
      match pat with
      | None -> a, b, nexpr
      | Some pat -> EquSimple (mono_ident pat, nexpr_merge) :: a, b, nexpr
    end
  | CIdent v ->
    let sty = sty_for_ty texpr_type in
    let nexpr = {
      nexpr_desc = NIdent v;
      nexpr_type = sty;
      nexpr_clock = texpr_clock;
      nexpr_loc = texpr_loc;
    } in
    let nexpr_merge = {
      nexpr_merge_desc = NExpr nexpr;
      nexpr_merge_type = sty;
      nexpr_merge_clock = texpr_clock;
      nexpr_merge_loc = texpr_loc;
    } in
    begin
      match pat with
      | None -> a, b, nexpr
      | Some pat -> EquSimple (mono_ident pat, nexpr_merge) :: a, b, nexpr
    end
  | CPair (_, _) -> assert false
  | CFby (c, e) ->
    let v, b = match pat with
      | Some ({ pat_desc = Ast_typed.VIdent (v, _); } ) -> v, b
      | None -> new_var b texpr_type
      | Some _ -> assert false
    in
    let sty = sty_for_ty texpr_type in
    let nexpr = {
      nexpr_desc = NIdent v;
      nexpr_type = sty;
      nexpr_clock = texpr_clock;
      nexpr_loc = texpr_loc;
    } in
    let a, b, nexpr_e = normalize_expr a b None e in
    let v', b = new_var b texpr_type in
    let nexpr' = {
      nexpr_desc = NIdent v';
      nexpr_type = sty;
      nexpr_clock = texpr_clock;
      nexpr_loc = texpr_loc;
    } in
    let nexpr' = {
      nexpr_merge_desc = NExpr nexpr';
      nexpr_merge_type = sty;
      nexpr_merge_clock = texpr_clock;
      nexpr_merge_loc = texpr_loc;
    } in
    EquFby (v', c, nexpr_e) :: EquSimple(v, nexpr') :: a, b, nexpr

  | CBOp (op, e1, e2) ->
    let a, b, e1 = normalize_expr a b None e1 in
    let a, b, e2 = normalize_expr a b None e2 in
    let sty = sty_for_ty texpr_type in
    let nexpr = {
      nexpr_desc = NBOp(op, e1, e2);
      nexpr_type = sty;
      nexpr_clock = texpr_clock;
      nexpr_loc = texpr_loc;
    } in
    let nexpr_merge = {
      nexpr_merge_desc = NExpr nexpr;
      nexpr_merge_type = sty;
      nexpr_merge_clock = texpr_clock;
      nexpr_merge_loc = texpr_loc;
    } in
    begin
      match pat with
      | None -> a, b, nexpr
      | Some pat -> EquSimple (mono_ident pat, nexpr_merge) :: a, b, nexpr
    end
  | CUOp (op, e1) ->
    let a, b, e1 = normalize_expr a b None e1 in
    let sty = sty_for_ty texpr_type in
    let nexpr = {
      nexpr_desc = NUOp(op, e1);
      nexpr_type = sty;
      nexpr_clock = texpr_clock;
      nexpr_loc = texpr_loc;
    } in
    let nexpr_merge = {
      nexpr_merge_desc = NExpr nexpr;
      nexpr_merge_type = sty;
      nexpr_merge_clock = texpr_clock;
      nexpr_merge_loc = texpr_loc;
    } in
    begin
      match pat with
      | None -> a, b, nexpr
      | Some pat -> EquSimple (mono_ident pat, nexpr_merge) :: a, b, nexpr
    end
  | CApp (name, args, every) ->
    let a, b, vl = normalize_paired a b args in
    let a, b, ev = normalize_expr a b None every in
    let pat, b = match pat with
      | Some pat -> pat, b
      | None ->
        let vl, b = come_up_vars b texpr_type in
        { pat_desc = vl; pat_loc = texpr_loc; }, b
    in
    let nexpr = match pat with
      | { pat_desc = Ast_typed.VIdent(v, ty); } ->
        {
          nexpr_desc = NIdent(v);
          nexpr_type = sty_for_ty ty;
          nexpr_clock = texpr_clock;
          nexpr_loc = texpr_loc;
        }
      | _ ->
        {
          nexpr_desc = NConst(Ast_typed.CNil);
          nexpr_type = Obj.magic(StyBool);
          nexpr_clock = texpr_clock;
          nexpr_loc = texpr_loc;
        } (* we *know* that this thing can never be used. *)
    in
    EquApp(pat, name, vl, ev) :: a, b, nexpr
  | CWhen (_, _, _) -> assert false
  | CMerge (i, _) -> assert false
and normalize_var: type a. Ast_normalized.nequation list -> Ast_clocked.node_local -> a Ast_clocked.cexpr ->
  Ast_normalized.nequation list * Ast_clocked.node_local * Ast_typed.ident = fun a b expr ->
  match expr.Ast_clocked.texpr_desc with
  | Ast_clocked.CIdent v -> a, b, v
  | _ ->
    let var, b = new_var b expr.Ast_clocked.texpr_type in
    let a, b, _ = normalize_expr a b (Some Ast_clocked.{ pat_desc = Ast_typed.VIdent(var, expr.texpr_type); pat_loc = expr.texpr_loc; }) expr
    in
    a, b, var

and normalize_paired: type a. Ast_normalized.nequation list -> Ast_clocked.node_local -> a Ast_clocked.cexpr ->
  Ast_normalized.nequation list * Ast_clocked.node_local * a Ast_typed.var_list = fun a b expr ->
  let Ast_clocked.{ texpr_desc; texpr_type; texpr_clock; texpr_loc } = expr in
  match texpr_desc with
  | Ast_clocked.CPair(e1, e2) ->
    let a, b, ne1 = normalize_var a b e1 in
    let a, b, np2 = normalize_paired a b e2 in
    a, b, Ast_typed.VTuple(ne1, e1.Ast_clocked.texpr_type, np2)
  | _ ->
    let a, b, e = normalize_var a b expr in
    a, b, Ast_typed.VIdent(e, texpr_type)



let normalize_node (Ast_clocked.Node desc) =
  let open Ast_clocked in
  let eqs, n_local = List.fold_left normalize_eqs ([], desc.n_local) desc.n_eqs  in
  Ast_normalized.NNode Ast_normalized.{
      n_name = desc.n_name;
      n_input = desc.n_input;
      n_output = desc.n_output;
      n_local = n_local;
      n_eqs = eqs;
      n_loc = desc.n_loc;
    }


let normalize_file = List.map normalize_node
