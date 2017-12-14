(**
 * Transforming match constructs
 *
 * Assumes no automaton
 *
 * Implementation is quadratic but performance should not be an issue
 *)

open Ast_ext

exception Error of location * string

exception NotShared of location * string

let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

let pair_to_var_decl (id, ty) ={
  v_name = id ;
  v_type = ty ;
  v_shared = false ;
  v_init = None ;
  v_loc = dummy_loc
}

let mk_eq id e pos = {eq_desc = EEq ({pat_desc = PIdent id ; pat_loc = (pos, pos)}, e) ; eq_loc = (pos, pos)}

module SSet = Set.Make(String)

module VarMap = Map.Make(struct
    type t = ident
    let compare = compare
  end)


let rec split4 = function
  | [] -> ([], [], [], [])
  | (a, b, c, d) :: q -> let (p, q, r, s) = split4 q in
    (a :: p, b :: q, c :: r, d :: s)

let flatt_set = List.fold_left (fun s t -> SSet.union s t) SSet.empty
let flatt_set = List.fold_left (fun s t -> VarMap.union (fun x a b -> Some a) s t) VarMap.empty

(* get id list from pattern *)
let get_def_pat pat =
  let rec g = function
    | PIdent id -> SSet.singleton id
    | PTuple l -> List.fold_left (fun s p -> SSet.union (g p) s) SSet.empty l
  in g pat.pat_desc


(* substitute x -> (x when ck)
 *            last x -> (pre x when ck)     for x free variable
 *            y -> y_suf                    for y in def
 *)

let rec subst_expr = assert false
 
let rec subst_eq shared ck def suf eq = match eq.eq_desc with
  | EEq (pat, e) -> let e, ndef = subst_expr e in 
    {eq with eq_desc = EEq(pat, e)}, SSet.diff ndef (get_def_pat pat)
  | _ -> assert false (*TODO*)

and subst_eqs shared ck def suf eqs = assert false 

let subst_match shared ck def h = 
  let (eqs, ndef) = subst_eqs shared ck def h.m_name h.m_eqs in
  let proj id =
    let init = try (
      match (VarMap.find id shared) with
      | (ty, None) -> CNil
      | (ty, Some c) -> c )
      with Not_found -> raise (NotShared (dummy_loc, id)) in
    {expr_desc = EFby(init, {expr_desc = EIdent id; expr_loc = dummy_loc}) ; expr_loc = dummy_loc}
  in
  let eq_ndef = List.map (fun id -> mk_eq (id ^ h.m_name) (proj id) Lexing.dummy_pos) (SSet.elements ndef) in
  eqs @ eq_ndef


(* returns (new_eqs, fresh_names, def(eq)) *)
let rec tr_eq shared eq = match eq.eq_desc with 
  | EEq (pat, _) -> [eq], [], get_def_pat pat, shared
  | EReset (eqs, e) -> let eqs', fresh, def, shared = (tr_eqs shared) eqs in
    [{eq with eq_desc = EReset (eqs', e)}], fresh, def, shared
  | EMatch (id, hs) -> let hs', fresh, def, shared = tr_match shared hs in

    let def_list = SSet.elements def in
    let rec get_new_vars fresh' def' = function
      | [] -> fresh', def'
      | h :: q -> let fresh' = List.fold_left (fun l id -> 
          (id^h.m_name, try fst (VarMap.find id shared)
                          with Not_found -> raise (NotShared (dummy_loc, id)) ) :: l ) fresh' def_list in
        let def' = List.fold_left (fun s id -> SSet.add (id^h.m_name) s) def' def_list in
        (*let shared' = List.fold_left ( fun m id -> 
            (try VarMap.add (id^h.m_name) (VarMap.find id shared) m  
             with Not_found -> raise (NotShared (dummy_loc, id)) ) ) shared' def_list in*)
        get_new_vars fresh' def' q
    in
    let new_fresh, new_def = get_new_vars fresh def hs in

    let eqs = List.flatten (List.map (subst_match shared id def) hs) in
    let merges = [] in

    merges @ eqs, new_fresh, new_def, shared
  | EAutomaton _ -> assert false

and tr_match shared hs =
  let tr_h h = let (eqs, f, d, shared) = tr_eqs shared h.m_eqs in
    ({h with m_eqs = eqs}, f, d, shared) in
  let (hs', fresh, def, shared) = split4 (List.map tr_h hs) in
  (hs', List.flatten fresh, flatt_set def, flatt_map shared)

and tr_eqs shared eqs = 
  let (eqs, fresh, def, shared) = split4 (List.map (tr_eq shared) eqs) in
  (List.flatten eqs, List.flatten fresh, flatt_set def, flatt_map shared)

let tr_node n = 
  let shared = List.fold_left 
                 (fun m vd -> if vd.v_shared then VarMap.add vd.v_name (vd.v_type, vd.v_init) m else m)
                 VarMap.empty n.n_local in (* contains shared variables mapped to their optional initial value *)
  let (eqs, fresh, _, _) = tr_eqs shared n.n_eqs in
  let defs = List.map pair_to_var_decl fresh in
  { n with n_eqs = eqs ; n_local = defs @ n.n_local}

let tr_file f = try 
    { f with f_nodes = List.map tr_node f.f_nodes }
  with NotShared (loc, id) -> raise (Error (loc, ("Variable "^id^" should be shared")))
