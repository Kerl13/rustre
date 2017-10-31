open Ast_typed

(* Equality type *)
type (_, _) eq = Refl: ('a, 'a) eq

(* Type to wrap the result from a search *)
type (_, _) res_node = Anode: ('a, 'b) node_desc * ('c, 'a) eq * ('d, 'b) eq -> ('c, 'd) res_node

exception Not_equal
exception Bad_type

(* Assuming a = b, return an equality witness. Raise Not_equal at runtime if the
 * precondition is not met. *)
let is_ty_equal : type a b. a ty -> b ty -> (b, a) eq = fun a b ->
  match a, b with
  | TyBool, TyBool -> Refl
  | TyNum a, TyNum b -> begin
      match a, b with
      | TyZ, TyZ -> Refl
      | TyReal, TyReal -> Refl
      | _ -> raise Not_equal
    end
  | _ -> raise Not_equal

(* Assuming a = b, return an equality witness. Raise Not_equal at runtime if the
 * precondition is not met. *)
let rec is_var_list_eq : type a b. a var_list -> b var_list -> (b, a) eq = fun a b ->
  match a, b with
  | VEmpty, VEmpty -> Refl
  | VIdent (a, tya), VIdent (b, tyb) ->
    if a <> b then raise Not_equal;
    is_ty_equal tya tyb
  | VTuple(a, tya, b), VTuple(c, tyc, d) ->
    if a <> c then raise Not_equal;
    let eq_b_d = is_var_list_eq b d in
    let eq_tya_tyc = is_ty_equal tya tyc in
    begin
      match eq_b_d, eq_tya_tyc with
      | Refl, Refl -> Refl
    end
  | _ -> raise Not_equal

(* If the idents are equal, get a witness for the equality of both 'a = 'c
 * and 'd = 'b.
 * Raises Bad_type if the idents are equal but the in/out types are not.
 * Returns None if the idents are different. *)
let is_equal (a:('a, 'b) tagged_ident) (b: ('c, 'd) tagged_ident) =
  match a, b with
  | Tagged(ina, outa, ida), Tagged(inb, outb, idb) ->
    if ida = idb then
      try
        Some (is_var_list_eq outa outb, is_var_list_eq ina inb)
      with
      | Not_equal -> raise Bad_type (* should never be fired if everything is well formed, it should fail during the typing *)
    else
      None

(* Looks for the definition of the tagged ident. Returns a wraped value
 * containing the node definition and equality witnesses between the node
 * definition types and the ident type. *)
let get_desc_res: file -> ('a, 'b) tagged_ident -> ('a, 'b) res_node = fun file id ->
  let c = List.fold_left (fun opt (Node n) ->
      match opt with
      | None -> begin
          match is_equal n.n_name id with
          | None -> None
          | Some (a, b) -> Some (Anode(n, b, a)) end
      | Some _ -> opt) None file in
  match c with
  | None -> raise Not_found
  | Some s -> s

let get_desc: file -> ('a, 'b) tagged_ident -> ('a, 'b) node_desc = fun file id ->
  let unwrap_res: type a b c d. (c, d) node_desc -> (a, c) eq -> (b, d) eq -> (a, b) node_desc = fun a b c ->
    match b, c with
    | Refl, Refl -> a
  in
  let res = get_desc_res file id in
  match res with
  | Anode (c_d_n, eq_c_a, eq_d_b) ->
    unwrap_res c_d_n eq_c_a eq_d_b
