open Ast_typed

type var_env = Var: 'a var_ident * 'a ty -> var_env
type var_list_wrapped = VarList: 'a var_list -> var_list_wrapped
type typed_ty_wrapped = TypedTy: 'a ty -> typed_ty_wrapped

module VarMap = Map.Make(struct
    type t = Ast_parsing.ident
    let compare = compare
  end)

let ty_to_typed_ty = function
  | Ast_parsing.TyBool -> TypedTy TyBool
  | Ast_parsing.TyInt -> TypedTy (TyNum TyZ)
  | Ast_parsing.TyReal -> TypedTy (TyNum TyReal)

exception Bad_type

let do_typing_const: type a. a ty -> Ast_parsing.const -> a const = fun ty a ->
  (match a with
   | Ast_parsing.CNil -> CNil
   | Ast_parsing.CInt a -> (match ty with
       | TyNum TyZ -> CInt a
       | TyNum TyReal -> CReal (float_of_int a)
       | _ -> raise Bad_type)
   | Ast_parsing.CReal a -> (match ty with
       | TyNum TyReal -> CReal a
       | _ -> raise Bad_type)
   | Ast_parsing.CBool a ->  (match ty with
       | TyBool -> CBool a
       | _ -> raise Bad_type))

let mono_type ty =
  match ty with
  | VIdent (_, myty) -> myty
  | _ -> raise Bad_type

let rec do_typing_expr: type a. var_env VarMap.t -> file -> a var_list -> Ast_parsing.expr -> a expr = fun env file ty expr ->
  let (descr: a expr_desc) = match let open Ast_parsing in expr.expr_desc with
    | Ast_parsing.EConst a ->
      let (c: a const) = do_typing_const (mono_type ty) a in
      EConst c
    | Ast_parsing.EIdent a ->
      let Var(var_name, var_ty) = VarMap.find a env in
      if TypedTy var_ty = TypedTy (mono_type ty) then
        EIdent var_name
      else raise Bad_type
    | Ast_parsing.ETuple [] ->
      raise Bad_type
    | Ast_parsing.ETuple (t::q) ->
      (match ty with
       | Ast_typed.VIdent (_,_) -> if q = [] then (??)
         else raise Bad_type
       | Ast_typed.VEmpty -> raise Bad_type
       | Ast_typed.VTuple (var_name, var_ty, b) -> (??)
       (* )(List.fold_left (fun expr_tuple expr ->
         { texpr_desc = EPair (assert false, assert false)); texpr_type =
         (do_typing_expr env file (VIdent (var_name, var_ty)) t) q).texpr_desc
       ) *))
    | Ast_parsing.EFby (a,b) ->
      EFby(do_typing_const (mono_type ty) a, do_typing_expr env file ty b)
    | Ast_parsing.EOp (_,_) -> (??)
    | Ast_parsing.EApp (_,_,_) -> (??)
    | Ast_parsing.EWhen (_,_,_) -> (??)
    | Ast_parsing.EMerge (_,_) -> (??)
  in
  {texpr_desc = descr; texpr_type = assert false; texpr_loc = Ast_parsing.(expr.expr_loc); }


let do_typing_equation env file eq =
  assert false

let do_typing_equations env file eqs =
  List.map (do_typing_equation env file) eqs

let do_typing_var_list k =
  List.rev k
  |> function
  | [] -> VarList VEmpty
  | (fst_var, fst_ty)::k ->
    let rec do_typing_var_list k var_list = match k with
      | [] -> var_list
      | (name, ty)::q ->
        let VarList v = var_list in
        let (name:string) = name in
        let TypedTy tt = ty_to_typed_ty ty in
        let v = VarList (VTuple(name, tt, v)) in
        do_typing_var_list q v
    in
    let TypedTy tt = ty_to_typed_ty fst_ty in
    let fst_var:string = fst_var in
    do_typing_var_list k (VarList (VIdent(fst_var, tt)))

let rec add_to_map: type a. a var_list -> var_env VarMap.t -> var_env VarMap.t =
  fun var_list (map: var_env VarMap.t) -> match var_list with
    | VIdent(a, t) -> VarMap.add a (Var(a, t)) map
    | VTuple(a, t, b) -> VarMap.add a (Var(a, t)) map
                         |> add_to_map b
    | VEmpty -> map

let do_typing_node (env:file) node =
  let VarList n_input = do_typing_var_list Ast_parsing.(node.n_input) in
  let VarList n_output = do_typing_var_list Ast_parsing.(node.n_output) in
  let VarList n_local = do_typing_var_list Ast_parsing.(node.n_output) in
  let n_name = Tagged(n_input, n_output, Ast_parsing.(node.n_name)) in
  let n_env = add_to_map n_input VarMap.empty
              |> add_to_map n_output
              |> add_to_map n_local
  in
  let n_eqs = do_typing_equations n_env env Ast_parsing.(node.n_eqs) in
  let node_desc = { n_name;
                    n_input;
                    n_output;
                    n_local = NodeLocal n_local;
                    n_eqs;
                    n_loc = Ast_parsing.(node.n_loc)} in
  Node node_desc :: env

let do_typing f =
  List.fold_left do_typing_node [] f
