open Ast_object
open Ast_normalized

let obc_varlist vl =
  let rec aux: type a. a Ast_typed.var_list -> var_list = function
    | Ast_typed.VIdent(a, ty) -> [a, Sty (Normalization.sty_for_ty ty)]
    | Ast_typed.VEmpty -> []
    | Ast_typed.VTuple(a, ty, b) ->
      (a, Sty (Normalization.sty_for_ty ty)) :: aux b
  in aux vl |> List.rev

let obc_node (NNode desc) =
  { memory = [];
    name = (let Ast_typed.Tagged(_, _, n) = desc.n_name in n);
    instances = [];
    reset = SSkip;
    step = obc_varlist desc.n_input, SSkip; }

let from_normalized file =
  List.map obc_node file
