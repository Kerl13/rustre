open Ast_typed

let rec var_list_fold : type b. ('a -> b var_ident -> 'a) -> 'a -> b var_list -> 'a
  = fun f acc v_list -> match v_list with
    | VIdent (x, _) -> f acc x
    | VEmpty -> acc
    | VTuple (x, _, v_list) -> var_list_fold f (f acc x) v_list
