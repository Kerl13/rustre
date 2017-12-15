(**
 * Transforming match constructs
 *
 * Assumes no automaton
 *)

open Ast_ext

exception Error of location * string

exception NotShared of location * string

let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

module SSet = Set.Make(String)

module VarMap = Map.Make(struct
    type t = ident
    let compare = compare
  end)

let id_to_var_decl vars id =
  let ty =
    let (l, d, _) = VarMap.split id vars in
    match d with
    | Some t -> t
    | None -> let (_, t) = VarMap.max_binding l in t (* take the type of the biggest known prefix *)
  in
  {
    v_name = id ;
    v_type = ty ;
    v_shared = false ;
    v_init = None ;
    v_loc = dummy_loc
  }

(* create equation id = e *)
let mk_eq id e pos = {eq_desc = EEq ({pat_desc = PIdent id ; pat_loc = (pos, pos)}, e) ; eq_loc = (pos, pos)}

let rec split3 = function
  | [] -> ([], [], [])
  | (a, b, c) :: q -> let (p, q, r) = split3 q in
    (a :: p, b :: q, c :: r)

let rec split4 = function
  | [] -> ([], [], [], [])
  | (a, b, c, d) :: q -> let (p, q, r, s) = split4 q in
    (a :: p, b :: q, c :: r, d :: s)

let flatt_set = List.fold_left (fun s t -> SSet.union s t) SSet.empty
let flatt_map = List.fold_left (fun s t -> VarMap.union (fun _ a _ -> Some a) s t) VarMap.empty

(* get id list from pattern *)
let get_def_pat pat =
  let rec g = function
    | PIdent id -> SSet.singleton id
    | PTuple l -> List.fold_left (fun s p -> SSet.union (g p) s) SSet.empty l
  in g pat.pat_desc


(* substitute x -> x_suf
 *            last x -> (pre x when suf(ck) )
 *
 *            ndef : shared variables with no defining equation in the considered handler,
 *            we need to add a definition x_suf = (x_init fby x) when suf(ck)
 *
 *            fv : free variables, we need to add an equation x_suf = x when suf(ck)
 *)

let rec subst_expr shared ck def suf e =
  let subst_id x =
    (if SSet.mem x def then
     try let _ = VarMap.find x shared in ()
     with Not_found -> raise (NotShared (e.expr_loc, x) )) ;
    x^suf, SSet.singleton x in

  let subst_expr = subst_expr shared ck def suf in

  let subst_exprs el =
    let el, fvl = List.split (List.map subst_expr el) in
    el, flatt_set fvl in

  let edesc, fv = match e.expr_desc with
  | EConst c -> EConst c, SSet.empty
  | EIdent x -> let x, fv = subst_id x in EIdent x, fv
  | ELast x -> (try let (_, init) = VarMap.find x shared in
      let init = match init with
      | None -> CNil
      | Some c -> c in
      EWhen({expr_desc = EFby(init, {expr_desc = EIdent x; expr_loc = e.expr_loc}); expr_loc = e.expr_loc}, suf, ck), SSet.singleton x
    with Not_found -> raise (NotShared (e.expr_loc, x) ))
  | ETuple l -> let l, fv = subst_exprs l in ETuple l, fv
  | EFby (c, e) -> let e, fv = subst_expr e in EFby(c, e), fv
  | EOp (o, l) -> let l, fv = subst_exprs l in EOp(o, l), fv
  | EApp (id, l, e) -> let id, fv = subst_id id in
    let l, fv' = subst_exprs l in
    let e, fv'' = subst_expr e in
    EApp (id, l, e), SSet.union fv (SSet.union fv' fv'')
  | EWhen (e, x, y) -> let e, fv = subst_expr e in EWhen(e, x, y), fv
  | EMerge (id, iel) -> let il, el = List.split iel in
    let el, fv = subst_exprs el in
    EMerge (id, List.combine il el), fv
  in
  {e with expr_desc = edesc}, fv

let subst_pat suf pat =
  let rec g = function
    | PIdent id -> PIdent (id^suf)
    | PTuple l -> PTuple (List.map g l)
  in {pat with pat_desc = g pat.pat_desc}

let rec subst_eq shared ck def suf eq = match eq.eq_desc with
  | EEq (pat, e) -> let ndef = SSet.diff def (get_def_pat pat) in
    let e, fv = subst_expr shared ck def suf e in
    {eq with eq_desc = EEq(subst_pat suf pat, e)}, ndef, fv
  | EReset (eqs, e) -> let (eqs, ndef, fv) = subst_eqs shared ck def suf eqs in
    let e, fv' = subst_expr shared ck def suf e in
    {eq with eq_desc = EReset (eqs, e)}, ndef, SSet.union fv fv'
  | _ -> assert false (* recursive matchs should have been transformed away *)

and subst_eqs shared ck def suf eqs =
  let eqs, ndefl, fvl = split3 (List.map (subst_eq shared ck def suf) eqs) in
  eqs, flatt_set ndefl, flatt_set fvl

(* apply substitutions and return equations for newly defined variables *)

let subst_match shared ck def h =
  let (eqs, ndef, fv) = subst_eqs shared ck def h.m_name h.m_eqs in
  let proj id =
    let init = try (
      match (VarMap.find id shared) with
      | (_, None) -> CNil
      | (_, Some c) -> c )
      with Not_found -> raise (NotShared (dummy_loc, id)) in
    {expr_desc = EFby(init, {expr_desc = EIdent id; expr_loc = dummy_loc}) ; expr_loc = dummy_loc}
  in
  let eq_ndef = List.map (fun id -> mk_eq (id ^ h.m_name) (proj id) Lexing.dummy_pos) (SSet.elements ndef) in
  let on id = {expr_desc = EWhen({expr_desc = EIdent id; expr_loc = dummy_loc}, h.m_name, ck) ; expr_loc = dummy_loc} in
  let eq_fv = List.map (fun id -> mk_eq (id ^ h.m_name) (on id) Lexing.dummy_pos) (SSet.elements (SSet.diff fv def)) in
  let fresh_ndef = SSet.map (fun id -> (id ^ h.m_name)) ndef in
  let fresh_fv = SSet.map (fun id -> (id ^ h.m_name)) fv in
  eqs @ eq_ndef @ eq_fv, SSet.union fresh_ndef fresh_fv


(* returns (new_eqs, fresh_names, def(eq)) *)
let rec tr_eq shared eq = match eq.eq_desc with
  | EEq (pat, _) -> [eq], SSet.empty, get_def_pat pat, shared
  | EReset (eqs, e) -> let eqs', fresh, def, shared = (tr_eqs shared) eqs in
    [{eq with eq_desc = EReset (eqs', e)}], fresh, def, shared
  | EMatch (id, hs) ->
    let hs, fresh, def, shared = tr_match shared hs in (* first transform recursive matchs *)

    let eqsl, freshl = List.split (List.map (subst_match shared id def) hs) in (* apply substitutions recursively *)
    let eqs = List.flatten eqsl in
    let new_fresh = SSet.union fresh (flatt_set freshl) in

    (* add equations y = merge id (C1 -> y_C1) ... (Cn -> y_Cn) for all y in def(eqs) *)
    let mk_merge y =
      let l = List.map (fun h -> (h.m_name, {expr_desc = EIdent(y^h.m_name) ; expr_loc = dummy_loc} ) ) hs in
      let e = {expr_desc = EMerge(id, l) ; expr_loc = dummy_loc}  in
      {eq_desc = EEq({pat_desc = PIdent y ; pat_loc = dummy_loc}, e) ; eq_loc = dummy_loc}
    in
    let merges = List.map mk_merge (SSet.elements def) in 

    let def_list = SSet.elements def in
    let rec get_new_vars def' shared' = function
      | [] -> def', shared'
      | h :: q ->  
        let def' = List.fold_left (fun s id -> SSet.add (id^h.m_name) s) def' def_list in
        let shared' = List.fold_left ( fun m id ->
            (try VarMap.add (id^h.m_name) (VarMap.find id shared) m
             with Not_found -> raise (NotShared (dummy_loc, id)) ) ) shared' def_list in
        get_new_vars def' shared' q
    in
    let new_def, new_shared = get_new_vars def shared hs in

    merges @ eqs, new_fresh, new_def, new_shared
  | EAutomaton _ -> assert false

and tr_match shared hs =
  let tr_h h = let (eqs, f, d, shared) = tr_eqs shared h.m_eqs in
    ({h with m_eqs = eqs}, f, d, shared) in
  let (hs', fresh, def, shared) = split4 (List.map tr_h hs) in
  (hs', flatt_set fresh, flatt_set def, flatt_map shared)

and tr_eqs shared eqs =
  let (eqs, fresh, def, shared) = split4 (List.map (tr_eq shared) eqs) in
  (List.flatten eqs, flatt_set fresh, flatt_set def, flatt_map shared)

let tr_node n =
  let shared = List.fold_left
                 (fun m vd -> if vd.v_shared then VarMap.add vd.v_name (vd.v_type, vd.v_init) m else m)
                 VarMap.empty n.n_local in (* contains shared variables mapped to their optional initial value *)
  let (eqs, fresh, _, _) = tr_eqs shared n.n_eqs in
  let tr_var_decl vd = vd.Ast_ext.v_name, vd.Ast_ext.v_type in
  let vars = List.fold_left (fun m (x, y) -> VarMap.add x y m) VarMap.empty ((List.map tr_var_decl n.n_local) @ n.n_output @ n.n_input) in
  let defs = List.map (id_to_var_decl vars) (SSet.elements fresh) in
  { n with n_eqs = eqs ; n_local = defs @ n.n_local}

let tr_file f =
  try { f with f_nodes = List.map tr_node f.f_nodes }
  with NotShared (loc, id) -> raise (Error (loc, ("Variable "^id^" should be shared")))
