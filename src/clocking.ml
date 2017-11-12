open Ast_typed
open Ast_clocked

module type Clocking = sig
  val clock_file: Ast_typed.file -> Ast_clocked.file
end
(*
 * /!\ This is not done yet...
 *)

module Stupid = struct

  let rec clock_expr : type a. a expr -> a cexpr = fun exp ->
    { texpr_desc=(clock_expr_desc exp.texpr_desc); texpr_clock=(CSingle CBase); texpr_type=exp.texpr_type; texpr_loc=exp.texpr_loc }

  and clock_expr_desc : type a. a expr_desc -> a cexpr_desc = fun exp ->
    match exp with
    | EConst c ->
      CConst c
    | EIdent v ->
      CIdent v
    | EPair (e1, e2) ->
      CPair (clock_expr e1, clock_expr e2)
    | EFby (c, e) ->
      CFby (c, clock_expr e)
    | EBOp (b, e1, e2) ->
      CBOp (b, clock_expr e1, clock_expr e2)
    | EUOp (b, e) ->
      CUOp (b, clock_expr e)
    | EApp (f, args, ev) ->
      CApp (f, clock_expr args, clock_expr ev)
    | EWhen (e, c, x) ->
      CWhen (clock_expr e, c, x)
    | EMerge (x, clauses) ->
      CMerge (x, List.map (fun (x, e) -> (x, clock_expr e)) clauses)



  let clock_node_desc : type a b. (a, b) Ast_typed.node_desc -> (a, b) node_desc = fun node -> failwith ""

  let clock_node (Node e:Ast_typed.node) : node =
    Node (clock_node_desc e)



  let clock_file (f:Ast_typed.file) : file = List.map clock_node f
end
