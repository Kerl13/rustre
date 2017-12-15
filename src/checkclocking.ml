open Ast_clocked
type location = Ast_typed.location

module type Checkclocking = sig
  exception Error of location * string
  val check_clock_file: Ast_typed.file -> string -> Ast_clocked.file -> unit
  (** We must know which is the main node *)
end

module CheckW = struct
  exception Error of location * string

  module Vmap = Map.Make(struct
      type t = cvar
      let compare v1 v2 = v1.id - v2.id
  end)

  let rec head = function
    | CVar { id = _ ; def = Some c } -> head c
    | c -> c

  let rec ck_eq ck1 ck2 = match head ck1, head ck2 with
    | CBase, CBase -> true
    | COn (ck1, dc1, x1), COn (ck2, dc2, x2) -> x1 = x2 && dc1 = dc2 && ck_eq ck1 ck2
    | CVar v1, CVar v2 -> v1.id = v2.id
    | _, _ -> false

  let ct_eq (ct1:ct) (ct2:ct) = List.for_all2 ck_eq ct1 ct2

  let create_node_env (Node n:node) : string * ct * ct =
    let Ast_typed.Tagged(_, _, n_name) = n.n_name in
    let clock_in = Ast_typed_utils.var_list_map (fun x -> Smap.find x n.n_clocks) n.n_input in
    let clock_out = Ast_typed_utils.var_list_map (fun x -> Smap.find x n.n_clocks) n.n_output in
    (n_name, clock_in, clock_out)

  let rec cts_of_cexpr_list : type a. a cexpr_list -> ct list = fun cexpr_l -> match cexpr_l with
    | CLNil -> []
    | CLSing x -> [x.texpr_clock]
    | CLCons (x, xs) -> x.texpr_clock :: (cts_of_cexpr_list xs)


  let rec check_clock_expr : type a. (string * Ast_clocked.ct * Ast_clocked.ct) list
                                  -> Ast_clocked.ck Ast_clocked.Smap.t -> a cexpr -> unit =
    fun node_env ck_map cexpr ->
    let cexpr_clock = cexpr.texpr_clock in
    let loc = cexpr.texpr_loc in
    match cexpr.texpr_desc with
    | CConst _ -> ()
    | CIdent x ->
       if not (ct_eq [(Smap.find x ck_map)] cexpr_clock) then
         raise (Error (loc, Format.sprintf "check_clock_expr, Ident %s" x))
    | CFby (_, e) ->
       let clock_e = e.texpr_clock in
       if not (ct_eq cexpr_clock clock_e) then
         raise (Error (loc, Format.sprintf "check_clock_expr, Fby"))
       else
         check_clock_expr node_env ck_map e
    | CBOp (_, e1, e2) ->
       if not (ct_eq e1.texpr_clock e2.texpr_clock) then
         raise (Error (loc, Format.sprintf "check_clock_expr, BOp, ck e1 <> ck e2"))
       else if not (ct_eq e1.texpr_clock cexpr_clock) then
         raise (Error (loc, Format.sprintf "check_clock_expr, BOp, ck e <> ck e1"))
       else
         check_clock_expr node_env ck_map e1; check_clock_expr node_env ck_map e2
    | CUOp (_, e1) ->
       if not (ct_eq e1.texpr_clock cexpr_clock) then
         raise (Error (loc, Format.sprintf "check_clock_expr, UOp, ck e <> ck e1"))
       else
         check_clock_expr node_env ck_map e1
    | CApp (f, arg, every) ->
       let Ast_typed.Tagged (_, _, f_id) = f in
       let (_, f_in_ct, f_out_ct) = List.find (fun (x, _, _) -> x = f_id) node_env in
       let arg_cts = cts_of_cexpr_list arg in
       let subst_map =
         let rec inst m ck' ck = match head ck', head ck with
           | CBase, CBase -> m
           | COn (ck', dc', x'), COn (ck, dc, x) ->
             assert (dc' = dc) ;
             assert (x' = x) ;
             inst m ck' ck
           | CVar v, ck -> Vmap.add v ck m
           | _ -> assert false
         in
         let inst m (ct, ck) = match ct with
           | [ck'] -> inst m ck ck'
           | _ -> assert false
         in
         List.map2 (fun ct ck -> (ct, ck)) arg_cts f_in_ct
         |> List.fold_left inst Vmap.empty
       in
       let find x =
         try Vmap.find x subst_map
         with Not_found -> CVar x
       in
       let out_cks =
         let rec subst ck = match head ck with
           | CBase -> CBase
           | COn (ck, dc, x) -> COn (subst ck, dc, x)
           | CVar v -> find v
         in
         List.map subst f_out_ct
       in
       let ct_le =
         let rec ck_le ck1 ck2 = match head ck1, head ck2 with
           | CBase, CBase -> true
           | COn (ck1, dc1, x1), COn (ck2, dc2, x2) ->
             dc1 = dc2 && x1 = x2 && ck_le ck1 ck2
           | _, CVar _ -> true
           | _ ->
             false
         in List.for_all2 ck_le
       in
       if not (ct_le cexpr_clock out_cks) then
         raise (Error (loc, Format.sprintf "check_clock_expr, CApp")) ;
       check_clock_expr_list node_env ck_map arg;
       check_clock_expr node_env ck_map every
    | CWhen (e, c, x) ->
        let ck_e = match e.texpr_clock with
          | [ck_e] -> ck_e
          | _ -> assert false in
        let ck_x = Smap.find x ck_map in
        if not (ck_eq ck_e ck_x) then
          raise (Error (loc, Format.sprintf "check_clock_expr, CWhen, ck_e <> ck_x"))
        else if not (ct_eq cexpr_clock [COn (ck_e, c, x)]) then
          raise (Error (loc, Format.sprintf "check_clock_expr, CWhen, cexpr_clok <> COn(...)"))
        else
          check_clock_expr node_env ck_map e
    | CMerge (x, cases) ->
       let ck_x = Smap.find x ck_map in
       let _ = check_clock_match_cases ck_x x cases in
       if not (ct_eq cexpr_clock [ck_x]) then
         let _ = raise (Error (loc, Format.sprintf "check_clock_expr, CMerge, cexpr_clock <> [ck_x]")) in ()
       else
         let _ = List.map (fun (_, c) -> check_clock_expr node_env ck_map c) cases in ()

  and check_clock_match_cases : type a. ck -> string -> (string * a cexpr) list -> unit =
    fun ck x cases ->
    let _ = List.map (fun (dc, e) -> if not (ct_eq e.texpr_clock [COn(ck, dc, x)]) then
                               raise (Error (e.texpr_loc, Format.sprintf "check_clock_match_cases"))) cases in ()

  and check_clock_expr_list : type a. (string * Ast_clocked.ct * Ast_clocked.ct) list -> Ast_clocked.ck Ast_clocked.Smap.t -> a cexpr_list  -> unit =
    fun node_env ck_map e_list ->
    match e_list with
    | CLNil -> ()
    | CLSing e -> check_clock_expr node_env ck_map e
    | CLCons (e, es) ->
       check_clock_expr node_env ck_map e; check_clock_expr_list node_env ck_map es

  let ct_of_pattern : type a. Ast_clocked.ck Ast_clocked.Smap.t -> a Ast_typed.pattern -> ct =
    fun ck_map pat ->
    Ast_typed_utils.var_list_map (fun x -> Smap.find x ck_map) pat.Ast_typed.pat_desc


  let check_clock_eq node_env ck_map (Ast_clocked.Equ (pat, expr)) =
    let ct_pat = ct_of_pattern ck_map pat in
    let ct_expr = expr.texpr_clock in
    if not (ct_eq ct_pat ct_expr) then
      raise (Error (expr.texpr_loc, Format.sprintf "check_clock_eq"))
    else
      check_clock_expr node_env ck_map expr


  let check_var_list_eq l1 l2 =
    Ast_typed_utils.var_list_fold2 (fun acc x1 x2 -> acc && (x1 = x2)) true l1 l2

  let check_clock_node main node_env (Ast_typed.Node node_in) (Node node_out) =
    let Ast_typed.Tagged(_, _, node_in_name) = node_in.Ast_typed.n_name in
    let Ast_typed.Tagged(_, _, node_out_name) = node_out.n_name in
    assert(node_in_name = node_out_name);
    let in_input = node_in.Ast_typed.n_input in
    let out_input = node_out.n_input in
    let in_output = node_in.Ast_typed.n_output in
    let out_output = node_out.n_output in
    let in_loc = node_in.Ast_typed.n_loc in
    let out_loc = node_out.n_loc in
    let out_eqs = node_out.n_eqs in
    let Ast_typed.NodeLocal in_local = node_in.Ast_typed.n_local in
    let Ast_typed.NodeLocal out_local = node_out.n_local in
    let ck_map = node_out.n_clocks in
    let in_check =
      if(node_out_name = main) then
        Ast_typed_utils.var_list_for_all (fun x -> Smap.find x ck_map = CBase) in_input
      else
        true
    in
    let _ =
      if not (check_var_list_eq in_input out_input) then
        raise (Error (out_loc, Format.sprintf "node %s, in_input <> out_input" node_in_name))
      else if not (check_var_list_eq in_output out_output) then
        raise (Error (out_loc, Format.sprintf "node %s, in_output <> out_output" node_in_name))
      else if not (in_loc = out_loc) then
        raise (Error (out_loc, Format.sprintf "Node %s, in_loc <> out_loc" node_in_name))
      else if not (check_var_list_eq in_local out_local) then
        raise (Error (out_loc, Format.sprintf "node %s, in_local <> out_local" node_in_name))
      else if not in_check then
        raise (Error (out_loc, Format.sprintf "node %s, not in_check" node_in_name))
      else
        List.map (check_clock_eq node_env ck_map) out_eqs
    in ()

  let check_clock_file f_in main f_out =
    let node_env = List.rev (List.fold_left (fun acc x -> create_node_env x::acc) [] f_out.cf_nodes) in
    let _ = List.map2 (check_clock_node main node_env) f_in.Ast_typed.tf_nodes f_out.cf_nodes in ()
end


module Stupid = struct
  exception Error of location * string

  let check_clock_file _ _ _ = ()
end
