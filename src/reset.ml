(**
 * Transforming reset constructs
 *
 * Assumes no automaton, match and last
 *)

open Ast_ext

let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

let fresh_id =
  let counter = ref 0 in
  fun () ->
    incr counter; Format.sprintf "reset_%d" !counter

let fresh_var e = 
  let id = fresh_id () in
  ( id , {eq_desc = EEq ({pat_desc = PIdent id ; pat_loc = dummy_loc}, e) ; eq_loc = dummy_loc})

let id_to_var_decl id ={
  v_name = id ;
  v_type = TyBool ;
  v_shared = false ;
  v_init = None ;
  v_loc = dummy_loc
}

let make_or e x = 
  let e2 = {expr_desc = EIdent x ; expr_loc = dummy_loc} in
  { expr_desc = EOp (OpOr, [e; e2]) ; expr_loc = e.expr_loc }

let rec tr_expr (cond : ident) expr = 
  let ed = match expr.expr_desc with
  | EConst c -> EConst c
  | EIdent x -> EIdent x
  | ETuple el -> ETuple (List.map (tr_expr cond) el)
  | EOp (o, el) -> EOp(o, List.map (tr_expr cond) el)
  | EWhen (e, x, y) -> EWhen(tr_expr cond e, x, y)
  | EMerge (x, l) -> EMerge (x, List.map (fun (x, e) -> (x, tr_expr cond e)) l)
  | EApp (x, el, e) -> EApp(x, List.map (tr_expr cond) el, make_or (tr_expr cond e) cond)
  | EFby (c, e) -> let ec = {expr_desc = EConst c ; expr_loc = expr.expr_loc} in
    Ext_utils.if_then_else cond ec {expr_desc = EFby (c, tr_expr cond e) ; expr_loc = expr.expr_loc} (snd expr.expr_loc)
  | ELast _ -> assert false
  in {expr with expr_desc = ed}

let rec tr_eq (cond : ident option) eq = match eq.eq_desc with 
  | EEq (pat, e) -> let e = match cond with None -> e | Some x -> tr_expr x e in
    [{eq with eq_desc = EEq (pat, e)}], [] 
  | EReset (eqs, e) -> 
    let e = match cond with None -> e | Some x -> make_or (tr_expr x e) x in
    let (id, eq_res) = fresh_var e in
    let (eqs, defs) = tr_eqs (Some id) eqs in
    eq_res :: eqs, id :: defs 
  | _ -> assert false (*should have been removed in earlier passes *)

and tr_eqs cond eqs = 
  let (eqs, defs) = List.split (List.map (tr_eq cond) eqs) in
  (List.flatten eqs, List.flatten defs)
  

let tr_node n = 
  let (eqs, defs) = tr_eqs None n.n_eqs in
  let defs = List.map id_to_var_decl defs in
  { n with n_eqs = eqs ; n_local = defs @ n.n_local}

let tr_file f = { f with f_nodes = List.map tr_node f.f_nodes }
