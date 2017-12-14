open Ast_clocked
type location = Ast_typed.location

module type Checkclocking = sig
  exception Error of string
  val check_clock_file: Ast_typed.file -> string -> Ast_clocked.file -> unit
  (** We must know which is the main node *)
end
                          
module CheckW = struct
  exception Error of string

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

  let rec cts_of_cexpr_list : type a. a cexpr_list -> ct = fun cexpr_l -> match cexpr_l with
    | CLNil -> assert false
    | CLSing x -> x.texpr_clock
    | CLCons (x, xs) -> x.texpr_clock @ (cts_of_cexpr_list xs)


  let rec check_clock_expr : type a. (string * Ast_clocked.ct * Ast_clocked.ct) list
                                  -> Ast_clocked.ck Ast_clocked.Smap.t -> a cexpr -> bool =
    fun node_env ck_map cexpr ->
    let cexpr_clock = cexpr.texpr_clock in
    match cexpr.texpr_desc with
    | CConst c -> true
    | CIdent x -> ct_eq [(Smap.find x ck_map)] cexpr_clock
    | CFby (c, e) ->
       let clock_e = e.texpr_clock in
       ct_eq cexpr_clock clock_e && check_clock_expr node_env ck_map e
    | CBOp (_, e1, e2) ->
       ct_eq e1.texpr_clock e2.texpr_clock &&
         ct_eq e1.texpr_clock cexpr_clock  &&
           check_clock_expr node_env ck_map e1      &&
             check_clock_expr node_env ck_map e2
    | CUOp (_, e) ->
       ct_eq e.texpr_clock cexpr_clock && check_clock_expr node_env ck_map e
    | CApp (f, arg, every) ->
       let Ast_typed.Tagged (_, _, f_id) = f in
       let (f_vars, f_in_ct, f_out_ct) = List.find (fun (x, _, _) -> x = f_id) node_env in
       let arg_cts = cts_of_cexpr_list arg in 
       ct_eq cexpr_clock f_out_ct && 
         check_clock_expr_list node_env ck_map arg && check_clock_expr node_env ck_map every &&
           (List.fold_left2 (fun b c1 c2 -> b && ck_eq c1 c2) true f_in_ct arg_cts)
    | CWhen (e, c, x) ->
        let ck_e = match e.texpr_clock with
          | [ck_e] -> ck_e
          | _ -> assert false in
        let ck_x = Smap.find x ck_map in
        ck_eq ck_e ck_x && ct_eq cexpr_clock [COn (ck_e, c, x)] && check_clock_expr node_env ck_map e
    | CMerge (x, cases) ->
       let ck_x = Smap.find x ck_map in
       let check_cases = check_clock_match_cases node_env ck_map ck_x x cases in
       ct_eq cexpr_clock [ck_x] && (List.fold_left (fun b (i, c) -> b && check_clock_expr node_env ck_map c) true cases)
       
  and check_clock_match_cases : type a. (string * Ast_clocked.ct * Ast_clocked.ct) list -> Ast_clocked.ck Ast_clocked.Smap.t -> ck -> string -> (string * a cexpr) list -> bool =
    fun node_env ck_map ck x cases ->
    List.for_all (fun (dc, e) -> ct_eq e.texpr_clock [COn(ck, dc, x)])  cases
                      
  and check_clock_expr_list : type a. (string * Ast_clocked.ct * Ast_clocked.ct) list -> Ast_clocked.ck Ast_clocked.Smap.t -> a cexpr_list  -> bool =
    fun node_env ck_map e_list ->
    match e_list with
    | CLNil -> true
    | CLSing e -> check_clock_expr node_env ck_map e
    | CLCons (e, es) ->
       check_clock_expr node_env ck_map e && check_clock_expr_list node_env ck_map es
                                   
  let ct_of_pattern : type a. Ast_clocked.ck Ast_clocked.Smap.t -> a Ast_typed.pattern -> ct =
    fun ck_map pat ->
    Ast_typed_utils.var_list_map (fun x -> Smap.find x ck_map) pat.Ast_typed.pat_desc


  let check_clock_eq node_env ck_map (Ast_clocked.Equ (pat, expr)) =
    let ct_pat = ct_of_pattern ck_map pat in
    let ct_expr = expr.texpr_clock in
    ct_eq ct_pat ct_expr && check_clock_expr node_env ck_map expr 
         

  let check_var_list_eq l1 l2 =
    Ast_typed_utils.var_list_fold2 (fun acc x1 x2 -> acc && (x1 = x2)) true l1 l2
    
  let check_clock_node main node_env (Ast_typed.Node node_in) (Node node_out) =
    (* todo: currently, we do not check that the equations are the same before / after *)
    (* we would need to add something like:    && in_eqs = (List.map (fun Equ (pat, cexpr) -> Ast_typed.Equ (pat, x) out_eqs) *)
    let Ast_typed.Tagged(_, _, node_in_name) = node_in.Ast_typed.n_name in
    let Ast_typed.Tagged(_, _, node_out_name) = node_out.n_name in
    assert(node_in_name = node_out_name); 
    let in_input = node_in.Ast_typed.n_input in
    let out_input = node_out.n_input in
    let in_output = node_in.Ast_typed.n_output in
    let out_output = node_out.n_output in
    let in_loc = node_in.Ast_typed.n_loc in
    let out_loc = node_out.n_loc in
    let in_eqs = node_in.Ast_typed.n_eqs in
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
    check_var_list_eq in_input out_input
    && check_var_list_eq in_output out_output
    && in_loc = out_loc 
    && check_var_list_eq in_local out_local
    && in_check
    && List.for_all (check_clock_eq node_env ck_map) out_eqs
      
    
    
    
  let check_clock_file f_in main f_out =
    let node_env = List.rev (List.fold_left (fun acc x -> create_node_env x::acc) [] f_out.cf_nodes) in
    if not (List.for_all2 (check_clock_node main node_env) f_in.Ast_typed.tf_nodes f_out.cf_nodes) then
      raise (Error "clock checking")
    else
      ()
end

              
module Stupid = struct
  exception Error of string

  let check_clock_file _ _ _ = ()
end