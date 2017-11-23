open Ast_clocked

module type Clocking = sig
  val clock_file: Ast_typed.file -> Ast_clocked.file
end
(*
 * /!\ This is not done yet...
 *)
module Stupid = struct
  type env = (string, ct option) Hashtbl.t

           
  let rec clock_expr : type a. env -> a Ast_typed.expr -> a cexpr = fun _ _ -> assert false
                                                                  
  let clock_eq: env -> Ast_typed.equation -> Ast_clocked.equation = fun env ->
    fun (Ast_typed.Equ (a, b)) ->
    let c_pat = a in
    let c_expr = clock_expr env b in
    Equ (c_pat, c_expr)



  let rec untag : type a.  a Ast_typed.var_list -> Ast_parsing.ident list = fun l -> match l with
                                                                                     | Ast_typed.VIdent (x, _) -> [x]
                                                                                     | Ast_typed.VEmpty -> []
                                                                                     | Ast_typed.VTuple  (x, _, l') -> x::(untag l')

  let create_base_env (n:('a, 'b) Ast_typed.node_desc) : env =
    let Ast_typed.NodeLocal local_vars = n.Ast_typed.n_local in
    let var_in = untag n.Ast_typed.n_input in
    let var_out = untag n.Ast_typed.n_output in
    let var_local = untag local_vars in
    let env =  Hashtbl.create (List.length var_in + List.length var_out + List.length var_local) in
    (* To create H_p *)
    let _ = List.map (fun x -> Hashtbl.add env x (Some ([CBase]))) var_in in
    (* To create H_q *)
    let _ = List.map (fun x -> Hashtbl.add env x (Some ([CBase]))) var_out in
    (* To create H_r (not instantiated yet *)
    let _ = List.map (fun x -> Hashtbl.add env x None) var_local in
    env

  (** TODO: something more involved **)
  let clock_compatible (c1:ct) (c2: ct) = (c1 = c2)

  let is_compatible (env:env) (s:Ast_parsing.ident) (cl:ct) : bool =
    let current_val = Hashtbl.find env s in
    match current_val with
    | None -> Hashtbl.replace env s (Some cl); true
    | Some sc -> clock_compatible sc cl


  let clock_node_desc : type a b. (a, b) Ast_typed.node_desc -> (a, b) Ast_clocked.node_desc = fun node ->
    let n_name = node.Ast_typed.n_name
    and n_input = node.Ast_typed.n_input
    and n_output = node.Ast_typed.n_output
    and n_local = node.Ast_typed.n_local;
    and n_loc = node.Ast_typed.n_loc in

    let c_env = create_base_env node in
    let n_eqs = List.map (clock_eq c_env) node.Ast_typed.n_eqs in
    { n_name=n_name; n_input=n_input; n_output=n_output; n_local=n_local; n_loc=n_loc; n_eqs=n_eqs }

  let clock_node (Ast_typed.Node e:Ast_typed.node) : node =
    Node (clock_node_desc e)



  let clock_file (f:Ast_typed.file) : file = List.map clock_node f
end

                     
(* module Working = struct
 *   module V = struct
 *     type t = cvar
 *     let compare v1 v2 = Pervasives.compare v1.id v2.id
 *     let equal v1 v2 = v1.id = v2.id
 *     let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
 *   end
 * 
 *   let rec head t = match t with
 *     (\* todo: replace def by Some (head y) ? *\)
 *     | CVar {def=Some y; _ } -> head y
 *     | _ -> t
 * 
 *   let rec canon t = match head t with
 *     | COn (ck, c, x) -> COn (canon ck, c, x)
 *     | _ as t' -> t'
 * 
 * 
 *   exception ClockUnificationFailure of ck * ck
 * 
 *   let unification_error t1 t2 = raise (ClockUnificationFailure (canon t1, canon t2))
 * 
 *                               
 *   let rec occur (v:cvar) (t:ck) : bool = match head t with
 *     (\** checks if variable v appears in the definition of type t **\)
 *     | CVar x -> V.equal x v
 *     | CBase -> false
 *     | COn (t', _, x) -> occur v t'
 * 
 *   let rec unify (t1:ck) (t2:ck) : unit = match (head t1, head t2) with
 *     | CBase, CBase -> ()
 *     | COn (t1, c1, x1), COn (t2, c2, x2) -> if (c1 = c2 && x1 = x2) then
 *                                               unify t1 t2
 *                                             else
 *                                               unification_error t1 t2
 *     | CVar v1, CVar v2 when V.equal v1 v2 -> ()
 *     | CVar v1, r2 ->
 *        if occur v1 r2 then unification_error (CVar v1) r2;
 *        assert (v1.def = None);
 *        v1.def <- Some t2
 *     | r1, CVar v2 -> unify (CVar v2) r1
 *     | r1, r2 -> unification_error r1 r2
 * 
 *   module Vset = Set.Make(V)
 * 
 *   let rec fvars (t:ck) : Vset.t = match (head t) with
 *     (\* returns the set of free variables inside type t *\)
 *     | COn (t', _, _) -> fvars t'
 *     | CVar x  -> Vset.singleton x
 *     | _ -> Vset.empty
 * 
 *   type schema = { vars : Vset.t; ck : ck }
 * 
 *   module Smap = Map.Make(String)
 * 
 *   type env = {bindings: schema Smap.t; fvars : Vset.t}
 *   type node_env = ct * ct Smap.t
 * 
 *   let node_empty = Smap.empty            
 *   let empty = {bindings=Smap.empty; fvars=Vset.empty}
 * 
 *   let add (s:string) (t:ck) (e:env) : env =
 *     (\* add a new variable s of type t to the env *\)
 *     let v = { vars = Vset.empty; ck = t } in
 *     {bindings=Smap.add s v e.bindings; fvars=Vset.union e.fvars (fvars t)}
 * 
 *   let fvars_env (e:env) : Vset.t =
 *     Vset.filter (fun x -> x.def = None) e.fvars
 *     
 *     
 *   let add_gen (s:string) (t:ck) (e:env) : env =
 *     (\* add a new variable s of type t to the env, with generalization *\)
 *     let {bindings=b; fvars=f} = e in
 *     let s_vars = Vset.diff (fvars t) (fvars_env e) in
 *     let v = { vars = s_vars; ck = t } in
 *     {bindings=Smap.add s v b; fvars=f}
 * 
 *   module Vmap = Map.Make(V)
 * 
 * 
 *   let find (s:string) (e:env) : ck =
 *     let s_schema = Smap.find s e.bindings in
 *     let s = Vset.fold (fun v s -> Vmap.add v (CVar (V.create ())) s) s_schema.vars Vmap.empty in
 *     let rec subst (s:ck Vmap.t) (t:ck) = match head t with
 *       | CBase -> CBase
 *       | COn (t', c, x) -> COn (subst s t', c, x)
 *       | CVar x -> (try Vmap.find x s with Not_found -> CVar x)
 *     in subst s s_schema.ck
 * 
 *   let rec clock_expr : type a. node_env -> env -> a Ast_typed.expr -> env * a cexpr  =
 *     fun node_env env expr ->
 *     let env,texpr_desc, texpr_clock = clock_expr_desc node_env env expr.Ast_typed.texpr_desc in
 *     env, { texpr_desc; texpr_clock; texpr_type=expr.Ast_typed.texpr_type; texpr_loc=expr.Ast_typed.texpr_loc }
 * 
 *   and clock_expr_list: type a. node_env -> env -> a Ast_typed.expr_list -> a cexpr_list = fun node_env env ->
 *     function
 *     | Ast_typed.ELNil -> CLNil
 *     | Ast_typed.ELSing a -> CLSing (clock_expr node_env env a)
 *     | Ast_typed.ELCons (a, b) -> CLCons (clock_expr node_env env a, clock_expr_list node_env env b)
 * 
 *   and clock_expr_desc : type a. node_env -> env -> a Ast_typed.expr_desc -> (env * a cexpr_desc * ct) = fun node_env env ->
 *     function
 *     | Ast_typed.EConst c ->
 *        CConst c, CBase
 *     | Ast_typed.EIdent e ->
 *        CIdent e, find e env
 *     | Ast_typed.EFby (v, a) ->
 *        let a_desc = clock_expr env a in
 *        CFby (v, a_desc), a_desc.texpr_clock
 *     | Ast_typed.EBOp (b, e1, e2) -> failwith "ni"
 *     | Ast_typed.EUOp (u, e) -> failwith "ni"
 *     | Ast_typed.EApp (f, x, every) ->
 *     (\* todo *\)
 *        let tau_list = clock_expr_list env x in
 *        failwith "ni"
 *        (\* let every_desc = clock_expr env every in
 *         * (\\** currently f_desc is CBase -> CBase -> ... -> CBase... *\\)
 *         * let f_desc = failwith "ni" in
 *         * let alpha = CVar (V.create ()) in
 *         * (\\* todo: type CArrow should be related to clock_expr_list ? *\\)
 *         * unify f_desc (CArrow (tau_list@[alpha]));
 *         * CApp (f_desc, x, every_desc), alpha *\)
 *     | Ast_typed.EWhen (a, c, x) ->
 *        let a_desc = clock_expr env a in
 *        let x_ck = find x env in
 *        unify x_ck a_desc.texpr_clock;
 *        CWhen (a_desc, c, x), COn (a_desc.texpr_clock, c, x)
 *     | Ast_typed.EMerge (x, l) ->
 *        let x_ck = find x env in
 *        let l_desc = List.map (fun (i, e) -> (i, clock_expr env e)) l in
 *        (\* todo: check exhaustivity of merge... *\)
 *        let subclocks = List.map  (fun (_, e) -> match e.texpr_clock with
 *                                                 | COn (ck, c, y) when y = x -> ck
 *                                                 | _ -> failwith "error in merge") l_desc in
 *        List.iter (unify x_ck) subclocks;
 *        CMerge (x, l_desc), x_ck
 *                                     
 *   let rec untag : type a. a Ast_typed.var_list -> Ast_parsing.ident list = function
 *     | Ast_typed.VIdent (x, _) -> [x]
 *     | Ast_typed.VEmpty -> []
 *     | Ast_typed.VTuple  (x, _, l') -> x::(untag l')
 * 
 *   let unfold_pattern: type a. a Ast_typed.pattern -> Ast_parsing.ident list = fun pat -> untag pat.Ast_typed.pat_desc
 * 
 *   let clock_eq: node_env -> env -> Ast_typed.equation -> env * Ast_clocked.equation = fun node_env env (Ast_typed.Equ (c_pat, expr)) ->
 *     let env, c_expr = clock_expr node_env env expr in
 *     List.combine (unfold_pattern c_pat) (c_expr.texpr_clock) |> List.iter (fun (ident, ct) -> unify (find ident env) ct);
 *     env, (Ast_clocked.Equ (c_pat, c_expr))
 *     
 *                                                                                                                      
 *   let create_base_env (n:('a, 'b) Ast_typed.node_desc) : env =
 *     let Ast_typed.NodeLocal local_vars = n.Ast_typed.n_local in
 *     let var_in = untag n.Ast_typed.n_input in
 *     let var_out = untag n.Ast_typed.n_output in
 *     let var_local = untag local_vars in
 *     List.fold_left (fun env s -> add s (CVar (V.create ())) env) empty (var_in @ var_local @ var_out)
 * 
 *                
 *   let clock_node_desc : type a b. node_env -> (a, b) Ast_typed.node_desc -> (a, b) Ast_clocked.node_desc = fun node_env node ->
 *     let n_name = node.Ast_typed.n_name
 *     and n_input = node.Ast_typed.n_input
 *     and n_output = node.Ast_typed.n_output
 *     and n_local = node.Ast_typed.n_local;
 *     and n_loc = node.Ast_typed.n_loc in
 *     
 *     let c_env = create_base_env node in
 *     let n_eqs =
 *       let rec aux n_env env eqs acc_new_eqs = match eqs with
 *         | [] -> acc_new_eqs
 *         | h::t -> let env, eq = clock_eq n_env env h in
 *                   aux n_env env t (eq::acc_new_eqs)
 *       in aux node_env c_env node.Ast_typed.n_eqs []
 *     in { n_name; n_input; n_output; n_local; n_loc; n_eqs }
 * 
 * 
 *      
 *   let clock_node (node_env,clocked) (Ast_typed.Node e) : node_env * file =
 *     let res = clock_node_desc node_env e in
 *     (\* TODO: màj de node_env avec les clocks de e *\)
 *     node_env, (Node res)::clocked
 *     
 *     
 *     
 *     
 *   let clock_file (f:Ast_typed.file) : file =
 *     let (_, file) = List.fold_left clock_node (node_empty,[]) f in
 *     List.rev file
 * end *)
