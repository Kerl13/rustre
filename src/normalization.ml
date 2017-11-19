let new_var_ =
  let counter = ref 0 in
  fun () ->
    incr counter; Format.sprintf "var%d" !counter

let new_var (Ast_typed.NodeLocal b) ty =
  let id = new_var_ () in
  id, Ast_typed.NodeLocal (Ast_typed.VTuple (id, ty, b))

let rec come_up_vars: type a. 'b -> a Ast_typed.compl_ty -> a Ast_typed.var_list * 'b =
  fun b ty ->
    match ty with
    | Ast_typed.TyNil -> Ast_typed.VEmpty, b
    | Ast_typed.TyPair(tya, vty) ->
      let vl, b = come_up_vars b vty in
      let a, b = new_var b tya in
      Ast_typed.VTuple(a, tya, vl), b

    | Ast_typed.TySing ty ->
      let a, b = new_var b ty in
      Ast_typed.VIdent(a, ty), b

let mono_ident Ast_typed.{ pat_desc; _ } =
  match pat_desc with
  | Ast_typed.VIdent(v, _) -> v

type _ expr_or_app =
  | Expr: 'a Ast_normalized.nexpr -> 'a Ast_typed.ty expr_or_app
  | App: ('a * 'b) expr_or_app
  | Unit: unit expr_or_app

let rec normalize_eqs (a, b) (Ast_clocked.Equ (pat, expr)) =
  let a, b, _ = normalize_expr a b (Some pat) expr in a, b

(* So the main problem with this function is that we cannot always return a
   `a nexpr`, sometimes that type is not inhabitated. Let's return Cnil instead ?? *)
and normalize_expr: type a. Ast_normalized.nequation list -> Ast_typed.node_local -> a Ast_typed.pattern option -> a Ast_clocked.cexpr ->
  Ast_normalized.nequation list * Ast_typed.node_local * a expr_or_app = fun a b pat
  Ast_clocked.{ texpr_desc; texpr_type; texpr_clock; texpr_loc } ->
  let open Ast_normalized in
  let open Ast_clocked in
  match texpr_desc with
  | CConst c ->
    let Ast_typed.TySing ty = texpr_type in
    let nexpr = {
      nexpr_desc = NConst c;
      nexpr_type = ty;
      nexpr_clock = texpr_clock;
      nexpr_loc = texpr_loc;
    } in
    let nexpr_merge = {
      nexpr_merge_desc = NExpr nexpr;
      nexpr_merge_type = ty;
      nexpr_merge_clock = texpr_clock;
      nexpr_merge_loc = texpr_loc;
    } in
    begin
      match pat with
      | None -> a, b, Expr nexpr
      | Some pat -> EquSimple (mono_ident pat, nexpr_merge) :: a, b, Expr nexpr
    end
  | CIdent v ->
    let Ast_typed.TySing sty = texpr_type in
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
      | None -> a, b, Expr nexpr
      | Some pat -> EquSimple (mono_ident pat, nexpr_merge) :: a, b, Expr nexpr
    end
  | CFby (c, e) ->
    let Ast_typed.TySing ty = texpr_type in
    let v, b = match pat with
      | Some (Ast_typed.{ pat_desc = Ast_typed.VIdent (v, _); _ } ) -> v, b
      | None -> new_var b ty
    in
    let nexpr = {
      nexpr_desc = NIdent v;
      nexpr_type = ty;
      nexpr_clock = texpr_clock;
      nexpr_loc = texpr_loc;
    } in

    let a, b, Expr nexpr_e = normalize_expr a b None e in
    let v', b = new_var b ty in
    let nexpr' = {
      nexpr_desc = NIdent v';
      nexpr_type = ty;
      nexpr_clock = texpr_clock;
      nexpr_loc = texpr_loc;
    } in
    let nexpr' = {
      nexpr_merge_desc = NExpr nexpr';
      nexpr_merge_type = ty;
      nexpr_merge_clock = texpr_clock;
      nexpr_merge_loc = texpr_loc;
    } in
    EquFby (v', c, nexpr_e) :: EquSimple(v, nexpr') :: a, b, Expr nexpr

  | CBOp (op, e1, e2) ->
    let a, b, Expr e1 = normalize_expr a b None e1 in
    let a, b, Expr e2 = normalize_expr a b None e2 in
    let Ast_typed.TySing sty = texpr_type in
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
      | None -> a, b, Expr nexpr
      | Some pat -> EquSimple (mono_ident pat, nexpr_merge) :: a, b, Expr nexpr
    end
  | CUOp (op, e1) ->
    let a, b, Expr e1 = normalize_expr a b None e1 in
    let Ast_typed.TySing sty = texpr_type in
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
      | None -> a, b, Expr nexpr
      | Some pat -> EquSimple (mono_ident pat, nexpr_merge) :: a, b, Expr nexpr
    end
  | CApp (name, args, every) ->
    let open Ast_typed in
    let a, b, vl = normalize_list a b args in
    let a, b, Expr ev = normalize_expr a b None every in
    let (pat, b) = match pat with
      | Some pat -> pat, b
      | None ->
        let vl, b = come_up_vars b texpr_type in
        { pat_desc = vl; pat_loc = texpr_loc; }, b
    in
    (match pat.pat_desc with
     | Ast_typed.VIdent(v, ty) ->
       let (ty: a) = ty in
       let nexpr= {
         nexpr_desc = NIdent(v);
         nexpr_type = ty;
         nexpr_clock = texpr_clock;
         nexpr_loc = texpr_loc;
       } in
       assert false, b, Expr nexpr
     | Ast_typed.VTuple(_, _, _) ->
       EquApp(pat, name, vl, ev) :: a, b, App
     | Ast_typed.VEmpty ->
       EquApp(pat, name, vl, ev) :: a, b, Unit
    )
  | CWhen (_, _, _) -> assert false
  | CMerge (_, _) -> assert false


and normalize_var: type a. Ast_normalized.nequation list -> Ast_typed.node_local -> a Ast_typed.ty Ast_clocked.cexpr ->
  Ast_normalized.nequation list * Ast_typed.node_local * Ast_typed.ident = fun a b expr ->
  match expr.Ast_clocked.texpr_desc with
  | Ast_clocked.CIdent v -> a, b, v
  | _ ->
    let Ast_typed.TySing ty = expr.Ast_clocked.texpr_type in
    let var, b = new_var b ty in
    let a, b, _ = normalize_expr a b (Some Ast_typed.{ pat_desc = Ast_typed.VIdent(var, ty); pat_loc = expr.Ast_clocked.texpr_loc; }) expr
    in
    a, b, var

and normalize_list: type a. Ast_normalized.nequation list -> Ast_typed.node_local -> a Ast_clocked.cexpr_list ->
  Ast_normalized.nequation list * Ast_typed.node_local * a Ast_typed.var_list = fun a b expr ->
  match expr with
  | Ast_clocked.CLCons(t, q) ->
    let a, b, ne1 = normalize_var a b t in
    let a, b, np2 = normalize_list a b q in
    let Ast_typed.TySing ty = t.Ast_clocked.texpr_type in
    a, b, Ast_typed.VTuple(ne1, ty, np2)
  | Ast_clocked.CLSing t ->
    let Ast_typed.TySing ty = t.Ast_clocked.texpr_type in
    let a, b, ne1 = normalize_var a b t in
    a, b, Ast_typed.VIdent(ne1, ty)
  | Ast_clocked.CLNil ->
    a, b, Ast_typed.VEmpty



let normalize_node (Ast_clocked.Node desc) =
  let eqs, n_local = List.fold_left normalize_eqs ([], desc.Ast_clocked.n_local) desc.Ast_clocked.n_eqs  in
  Ast_normalized.NNode Ast_normalized.{
      n_name = desc.Ast_clocked.n_name;
      n_input = desc.Ast_clocked.n_input;
      n_output = desc.Ast_clocked.n_output;
      n_local = n_local;
      n_eqs = eqs;
      n_loc = desc.Ast_clocked.n_loc;
    }


let normalize_file = List.map normalize_node
