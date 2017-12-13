open Ast_typed

type var_env = Var: 'a var_ident * 'a ty -> var_env
type var_list_wrapped = VarList: 'a var_list -> var_list_wrapped
type typed_ty_wrapped = TypedTy: 'a ty -> typed_ty_wrapped
type compl_ty_wrapped = TypedComplTy: 'a compl_ty -> compl_ty_wrapped

module VarMap = Map.Make(struct
    type t = Ast_parsing.ident
    let compare = compare
  end)

module SSet = Set.Make(String)

exception Error of location * string
(** Generic type errot - the only error visible outside the module.
    Should not be raised during typing but at the end of the process.
*)

exception Expected_num of location
exception Node_undefined of string * location
exception Empty_merge of location
exception Redundant_merge of location * ident
exception Non_exhaustive_merge of location
exception Expected_type of typed_ty_wrapped * typed_ty_wrapped * location
exception Type_error_at of location
exception Shadowing_data_constructor of ident * ident * ident
exception UnboundVar of location * ident
exception UnboundDC of location * ident

(** Typing environment *)
module Env = struct
  type t = {
    typedefs : (ident * ident list) list ;
    rev_typedefs : ident VarMap.t ;
    bindings: var_env VarMap.t ;
  }

  let empty = { typedefs = [] ; rev_typedefs = VarMap.empty ; bindings = VarMap.empty }

  (* Bindings *****************************************)

  (** Add a binding of the form [x : τ] *)
  let add x ve (env : t) = { env with bindings = VarMap.add x ve env.bindings }

  (** Returns [Γ(x)] *)
  let find loc x env =
    try VarMap.find x env.bindings
    with Not_found -> raise (UnboundVar (loc, x))

  (** Empties [Γ] *)
  let reset_bindings (env : t) = { env with bindings = VarMap.empty }

  (* Type definitions *********************************)

  (** Add a new enumerated type named [name] with the data construtors [dcs] *)
  let add_typedef name dcs (env : t) = {
    typedefs = (name, dcs) :: env.typedefs ;
    rev_typedefs = List.fold_left (fun map dc -> VarMap.add dc name map) env.rev_typedefs dcs ;
    bindings = env.bindings
  }

  (** Returns the data constructors of the type [name] *)
  let get_dcs name (env : t) =
    env.typedefs
    |> List.find (fun (name', _) -> name' = name)
    |> snd

  (** Returns the type name of the data constructor [dc] *)
  let get_type_name loc dc (env : t) =
    try VarMap.find dc env.rev_typedefs
    with Not_found -> raise (UnboundDC (loc, dc))

  (** Returns true iff [dc] is a data constructor of a known type *)
  let is_known_dc dc (env : t) = VarMap.mem dc env.rev_typedefs
end

let ty_to_typed_ty = function
  | Ast_parsing.TyBool -> TypedTy TyBool
  | Ast_parsing.TyInt -> TypedTy (TyNum TyZ)
  | Ast_parsing.TyReal -> TypedTy (TyNum TyReal)
  | Ast_parsing.TyEnum (name, dcs) -> TypedTy (TyEnum (name, dcs))


let do_typing_const: type a. Env.t -> a ty -> location -> Ast_parsing.const -> a const
  = fun env ty loc a ->
  (match a with
   | Ast_parsing.CNil -> CNil
   | Ast_parsing.CInt a -> (match ty with
       | TyNum TyZ -> CInt a
       | TyNum TyReal -> CReal (float_of_int a)
       | _ -> raise (Expected_type(TypedTy (TyNum TyZ), TypedTy(ty), loc)))
   | Ast_parsing.CReal a -> (match ty with
       | TyNum TyReal -> CReal a
       | _ -> raise (Expected_type(TypedTy (TyNum TyReal), TypedTy(ty), loc)))
   | Ast_parsing.CBool a -> (match ty with
       | TyBool -> CBool a
       | _ -> raise (Expected_type(TypedTy TyBool, TypedTy(ty), loc)))
   | Ast_parsing.CDataCons dc -> (match ty with
       | TyEnum (ty_name, _) when Env.get_type_name loc dc env = ty_name -> CDataCons dc
       | _ ->
         let expected_ty =
           let ty_name = Env.get_type_name loc dc env in
           TyEnum (ty_name, Env.get_dcs ty_name env)
         in
         raise (Expected_type(TypedTy expected_ty, TypedTy(ty), loc))))

let op_to_ty_op = function
  | Ast_parsing.OpAdd -> OpAdd
  | Ast_parsing.OpSub -> OpSub
  | Ast_parsing.OpMul -> OpMul
  | Ast_parsing.OpDiv -> OpDiv
  | Ast_parsing.OpMod -> OpMod
  | _ -> assert false

let op_to_ty_op_bool = function
  | Ast_parsing.OpImpl -> OpImpl
  | Ast_parsing.OpAnd -> OpAnd
  | Ast_parsing.OpOr -> OpOr
  | _ -> assert false

let op_to_ty_op_eq = function
  | Ast_parsing.OpEq -> OpEq
  | Ast_parsing.OpNeq -> OpNeq
  | _ -> assert false

let op_to_ty_op_cmp = function
  | Ast_parsing.OpLt -> OpLt
  | Ast_parsing.OpLe -> OpLe
  | Ast_parsing.OpGt -> OpGt
  | Ast_parsing.OpGe -> OpGe
  | _ -> assert false

let rec varlist_to_ty : type a. a var_list -> a compl_ty = function
  | Ast_typed.VIdent (_,a) -> TySing a
  | Ast_typed.VEmpty -> TyNil
  | Ast_typed.VTuple (_,a,b) -> TyPair(a, (varlist_to_ty b))

let varlist_eq: type a b. a var_list -> b var_list -> bool = fun a b ->
  TypedComplTy (varlist_to_ty a) = TypedComplTy (varlist_to_ty b)

(* Infer simple types (no pair) *)
let rec infer_type_opt (env : Env.t) (nodes : node list) (expr : Ast_parsing.expr) : typed_ty_wrapped option =
  let expr_loc = expr.Ast_parsing.expr_loc in
    match Ast_parsing.(expr.expr_desc) with
     | Ast_parsing.EConst c -> (match c with
         | Ast_parsing.CNil -> None
         | Ast_parsing.CInt _ -> None
         | Ast_parsing.CReal _ -> Some (TypedTy (TyNum TyReal))
         | Ast_parsing.CBool _ -> Some (TypedTy TyBool)
         | Ast_parsing.CDataCons dc ->
           let ty_name = Env.get_type_name expr_loc dc env in
           let dcs = Env.get_dcs ty_name env in
           Some (TypedTy (TyEnum (ty_name, dcs)))
       )
     | Ast_parsing.EIdent v ->
       let Var (_, t) = Env.find expr_loc v env in
       Some (TypedTy t)
     | Ast_parsing.ETuple _ -> raise (Type_error_at expr.Ast_parsing.expr_loc)
     | Ast_parsing.EFby (c,e) ->
       (match c with
        | Ast_parsing.CNil -> infer_type_opt env nodes e
        | Ast_parsing.CInt _ -> None
        | Ast_parsing.CReal _ -> Some (TypedTy (TyNum TyReal))
        | Ast_parsing.CBool _ -> Some (TypedTy TyBool)
        | Ast_parsing.CDataCons dc ->
          let ty_name = Env.get_type_name expr_loc dc env in
          let dcs = Env.get_dcs ty_name env in
          Some (TypedTy (TyEnum (ty_name, dcs))))
     | Ast_parsing.EOp (op,e) -> (match op with
         | Ast_parsing.OpAdd | Ast_parsing.OpSub | Ast_parsing.OpMul | Ast_parsing.OpDiv | Ast_parsing.OpMod -> (match e with
             | [a; b] ->
               (match infer_type_opt env nodes a with
                | Some s -> Some s
                | None -> infer_type_opt env nodes b)
             | _ -> raise (Type_error_at expr.Ast_parsing.expr_loc))
         | _ -> Some (TypedTy TyBool))
     | Ast_parsing.EApp (node_name,_,_) ->
       let node =
         try
           List.find (fun (Node desc) ->
             let Tagged(_, _, i) = desc.n_name in
             i = node_name) nodes
         with Not_found -> raise (Node_undefined (node_name, expr.Ast_parsing.expr_loc))
       in
       let Node node_desc = node in
       let Tagged(_, out_args, _) = node_desc.n_name in
       (match out_args with
        | VIdent(_, a) -> Some (TypedTy a)
        | _ -> raise (Type_error_at expr.Ast_parsing.expr_loc))
     | Ast_parsing.EWhen (a,_,_) -> infer_type_opt env nodes a
     | Ast_parsing.EMerge (_,e) ->
       if e = [] then raise (Empty_merge expr_loc)
       else List.hd e |> snd |> infer_type_opt env nodes

(* Infer simple types (no pair) *)
let infer_type (env : Env.t) (nodes : node list) (expr : Ast_parsing.expr) : typed_ty_wrapped =
  match infer_type_opt env nodes expr with
  | Some s -> s
  | _ -> TypedTy (TyNum TyZ)

let infer_type2 env loc nodes e =
  match e with
  | [a; b] ->
    (match infer_type_opt env nodes a with
     | Some s -> s
     | None -> match infer_type_opt env nodes b with
       | None -> TypedTy (TyNum TyZ)
       | Some s -> s)
  | _ -> raise (Type_error_at loc)

let rec do_typing_tuple: type a. Env.t -> location -> node list -> a var_list -> Ast_parsing.expr list -> a expr_list
  = fun env loc nodes ty expr ->
    begin
      match ty with
      | Ast_typed.VIdent (var_name, var_ty) ->
        begin match expr with
          | [t] ->
            ELSing (do_typing_expr env nodes (VIdent (var_name, var_ty)) t)
          | _ -> raise (Type_error_at loc)
        end
      | Ast_typed.VEmpty -> (match expr with
          | [] -> ELNil
          | _::_ -> raise (Type_error_at loc))
      | Ast_typed.VTuple (var_name, var_ty, b) ->
        begin match expr with
          | t::q ->
            let p1 = do_typing_expr env nodes (VIdent (var_name, var_ty)) t in
            let p2 = do_typing_tuple env loc nodes b q in
            ELCons(p1, p2)
          | [] -> raise (Type_error_at loc)
        end
    end

and binary_expr: type a b. Env.t -> node list -> a ty -> b ty -> (a, b) binop -> Ast_parsing.expr list -> b ty expr_desc =
  fun env nodes ty_in _ op exprs ->
    match exprs with
    | [a; b] ->
      let e1 = do_typing_expr env nodes (VIdent("", ty_in)) a in
      let e2 = do_typing_expr env nodes (VIdent("", ty_in)) b in
      EBOp(op, e1, e2)
    | _ -> (* should not go through parsing *) assert false

and do_typing_expr: type a. Env.t -> node list -> a var_list -> Ast_parsing.expr -> a expr
  = fun env nodes ty expr ->
  let expr_loc = expr.Ast_parsing.expr_loc in
  let (descr, ty): (a expr_desc * a compl_ty) = match expr.Ast_parsing.expr_desc with
    | Ast_parsing.EConst a ->
      (match ty with
       | VIdent(_, ty) ->
         let c = do_typing_const env ty expr_loc a in
         let (c: a expr_desc) = EConst c in
         let (ty: a compl_ty) = TySing ty in
         c, ty
       | _ -> raise (Type_error_at expr_loc))
    | Ast_parsing.EIdent a ->
      (match ty with
       | VIdent(_, ty) ->
         let Var(var_name, var_ty) = Env.find expr_loc a env in
         if TypedTy var_ty = TypedTy ty then
           let (ty: a compl_ty) = TySing ty in
           EIdent var_name, ty
         else  raise (Type_error_at expr_loc)
       | _ -> raise (Type_error_at expr_loc))
    | Ast_parsing.ETuple _ ->
      raise (Type_error_at expr_loc)
    | Ast_parsing.EFby (a,b) ->
      (match ty with
       | VIdent(_, ty2) ->
         EFby (do_typing_const env ty2 expr_loc a, do_typing_expr env nodes ty b), TySing ty2
       | _ -> raise (Type_error_at expr_loc))
    | Ast_parsing.EOp (op, exprs) -> (match op with
        | Ast_parsing.OpAdd | Ast_parsing.OpSub | Ast_parsing.OpMul
        | Ast_parsing.OpDiv | Ast_parsing.OpMod ->
          (match ty with
           | VIdent(_, TyNum t) ->
             binary_expr env nodes (TyNum t) (TyNum t) (op_to_ty_op op) exprs, TySing (TyNum t)
           | _ -> raise (Expected_num expr_loc))
        | Ast_parsing.OpLt
        | Ast_parsing.OpLe
        | Ast_parsing.OpGt
        | Ast_parsing.OpGe ->
          (match ty with
           | VIdent(_, TyBool) ->
             let a = List.hd exprs (* hd exists or should not go through parsing *) in
             let TypedTy inf_type = infer_type env nodes a in
             (match inf_type with
              | Ast_typed.TyNum _ ->
                binary_expr env nodes inf_type TyBool (op_to_ty_op_cmp op) exprs, TySing TyBool
              | _ -> raise (Type_error_at expr_loc))
           | _ -> raise (Type_error_at expr_loc))
        | Ast_parsing.OpEq
        | Ast_parsing.OpNeq ->
          (match ty with
           | VIdent (_, TyBool) ->
             let TypedTy inf_type = infer_type2 env expr_loc nodes exprs in
             binary_expr env nodes inf_type TyBool (op_to_ty_op_eq op) exprs, TySing TyBool
           | _ -> raise (Type_error_at expr_loc))
        | Ast_parsing.OpAnd | Ast_parsing.OpOr | Ast_parsing.OpImpl ->
            (match ty with
             | VIdent (_, TyBool) ->
               binary_expr env nodes TyBool TyBool (op_to_ty_op_bool op) exprs, TySing TyBool
             | _ -> raise (Type_error_at expr_loc))
        | Ast_parsing.OpNot ->
            (match ty with
             | VIdent (_, TyBool) ->
               let e = match exprs with
                 | [e] -> e
                 | _ -> assert false
               in
               EUOp (OpNot, do_typing_expr env nodes ty e), TySing TyBool
             | _ -> raise (Type_error_at expr_loc)))
    | Ast_parsing.EApp (node_name, args, every) ->
      begin
        try
          let node = List.find (fun (Node desc) ->
              let Tagged(_, _, i) = desc.n_name in
              i = node_name) nodes in
          let Node node_desc = node in
          let Tagged(in_args, out_args, _) = node_desc.n_name in
          let in_expr = do_typing_tuple env expr_loc nodes in_args args in
          let every_expr = do_typing_expr env nodes (VIdent("", TyBool)) every in
          if varlist_eq out_args ty then
            EApp (Tagged(in_args, ty, node_name), in_expr, every_expr), varlist_to_ty ty
          else raise (Type_error_at expr_loc)
        with
        | Not_found -> raise (Node_undefined (node_name, expr_loc))
      end
    | Ast_parsing.EWhen (e1,var,constructor) ->
      begin
        match ty with
        | VIdent(_, _) ->
          let e = do_typing_expr env nodes ty e1 in
          EWhen(e, var, constructor), e.texpr_type
        | _ -> raise (Type_error_at expr_loc)
      end
    | Ast_parsing.EMerge (var, id_exprs) ->
      match ty with
      | VIdent(_, _) ->
        let Var (_, ty_x) = Env.find expr_loc var env in
        let expected_dcs = begin match ty_x with
          | TyBool -> SSet.of_list ["True"; "False"]
          | TyEnum (_, dcs) -> SSet.of_list dcs
          | _ -> raise (Type_error_at expr_loc)
        end in
        let seen_dcs = SSet.empty in
        let seen_dcs, e = List.fold_left (fun (seen, cases) (id, e) ->
            if SSet.mem id seen then raise (Redundant_merge (expr_loc, id));
            if not (SSet.mem id expected_dcs) then raise (Type_error_at expr_loc);
            (SSet.add id seen), (id, do_typing_expr env nodes ty e) :: cases
          ) (seen_dcs, []) id_exprs in
        let missing = SSet.diff expected_dcs seen_dcs in
        if not (SSet.is_empty missing) then
          raise (Non_exhaustive_merge expr_loc);
        EMerge(var, e), (List.hd e |> snd).texpr_type
      | _ -> raise (Type_error_at expr_loc)
  in
  {texpr_desc = descr; texpr_type = ty; texpr_loc = expr_loc }


let do_typing_equation env nodes eq =
  let loc = eq.Ast_parsing.eq_pat.Ast_parsing.pat_loc in
  let pat_desc_to_var_list = function
    | Ast_parsing.PIdent v ->
      let Var(id, ty) = Env.find loc v env in
      VarList (VIdent(id, ty))
    | Ast_parsing.PTuple tuple ->
      let (t, q) = match List.rev tuple with
        | Ast_parsing.PIdent t :: q -> t, q
        | _ -> raise (Type_error_at loc)
      in
      let Var(id, ty) = Env.find loc t env in
      let v = VarList (VIdent(id, ty)) in
      List.fold_left (fun (VarList vl) pat_desc ->
          match pat_desc with
          | Ast_parsing.PIdent v ->
            let Var(id, ty) = Env.find loc v env in
            VarList (VTuple(id, ty, vl))
          | _ -> raise (Type_error_at loc)) v q
  in

  let VarList var_list = pat_desc_to_var_list eq.Ast_parsing.eq_pat.Ast_parsing.pat_desc in
  let pattern = { pat_desc = var_list; pat_loc = eq.Ast_parsing.eq_pat.Ast_parsing.pat_loc } in
  Equ (pattern, do_typing_expr env nodes var_list eq.Ast_parsing.eq_expr)

let do_typing_equations env nodes eqs =
  List.map (do_typing_equation env nodes) eqs

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

let rec add_to_env : type a. a var_list -> Env.t -> Env.t
  = fun var_list env -> match var_list with
    | VIdent(a, t) -> Env.add a (Var (a, t)) env
    | VTuple(a, t, b) -> add_to_env b (Env.add a (Var(a, t)) env)
    | VEmpty -> env

let do_typing_node (env:Env.t) (nodes:node list) (node:Ast_parsing.node) =
  let VarList n_input = do_typing_var_list node.Ast_parsing.n_input in
  let VarList n_output = do_typing_var_list node.Ast_parsing.n_output in
  let VarList n_local = do_typing_var_list node.Ast_parsing.n_local in
  let n_name = Tagged(n_input, n_output, node.Ast_parsing.n_name) in
  let env = env |> Env.reset_bindings
                |> add_to_env n_input
                |> add_to_env n_output
                |> add_to_env n_local
  in
  let n_eqs = do_typing_equations env nodes node.Ast_parsing.n_eqs in
  let node_desc = { n_name;
                    n_input;
                    n_output;
                    n_local = NodeLocal n_local;
                    n_eqs;
                    n_loc = node.Ast_parsing.n_loc} in
  Node node_desc :: nodes

let add_typedef env (name, dcs) =
  List.iter (fun dc ->
    if Env.is_known_dc dc env then
      let pos = Lexing.dummy_pos in
      raise (Shadowing_data_constructor (dc, name, Env.get_type_name (pos, pos) dc env))
  ) dcs;
  Env.add_typedef name dcs env

let do_typing f =
  let typedefs = f.Ast_parsing.f_typedefs in
  let env = List.fold_left add_typedef Env.empty typedefs in
  let nodes =
    f.Ast_parsing.f_nodes
    |> List.fold_left (do_typing_node env) []
    |> List.rev
  in { tf_typedefs = typedefs ; tf_nodes = nodes }
let do_typing f =
  let sprintf = Format.sprintf in
  let error loc msg = raise (Error (loc, msg)) in
  try do_typing f
  with
  | Expected_num loc ->
    let msg = "Value with a numeric type expected" in
    error loc msg
  | Node_undefined (n, loc) ->
    let msg = sprintf "Undefined node %s" n in
    error loc msg
  | Empty_merge loc ->
    let msg = "Merge without any clause" in
    error loc msg
  | Redundant_merge (loc, dc) ->
    let msg = sprintf "Redudant merge clause: (%s -> …)" dc in
    error loc msg
  | Non_exhaustive_merge loc ->
    let msg = "This merge is not exhaustive" in
    error loc msg
  | Expected_type(a, b, loc) ->
    let TypedTy a, TypedTy b = a, b in
    let msg = Format.asprintf "Got %a, expected %a" pp_ty a pp_ty b in
    error loc msg
  | Type_error_at loc ->
    let msg = "Type error" in
    error loc msg
  | Shadowing_data_constructor (dc, ty1, ty2) ->
    let msg = sprintf "Shadowning data constructor %s of type %s in the definition of %s" dc ty2 ty1 in
    let pos = Lexing.dummy_pos in
    error (pos, pos) msg
  | UnboundVar (loc, x) ->
    let msg = sprintf "Unbound var %s" x in
    error loc msg
  | UnboundDC (loc, dc) ->
    let msg = sprintf "Unbound data constructor %s" dc in
    error loc msg
