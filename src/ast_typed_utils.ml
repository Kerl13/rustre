open Ast_typed


let rec var_list_exists : type a. (Ast_parsing.ident -> bool) -> a var_list -> bool
  = fun p -> function
    | VEmpty -> false
    | VIdent (x, _) -> p x
    | VTuple (x, _, vl) -> p x || var_list_exists p vl

let rec var_list_for_all : type a. (Ast_parsing.ident -> bool) -> a var_list -> bool
  = fun p -> function
    | VEmpty -> true
    | VIdent (x, _) -> p x
    | VTuple (x, _, vl) -> p x && var_list_for_all p vl

                         
let rec var_list_fold : type b. ('a -> Ast_parsing.ident -> 'a) -> 'a -> b var_list -> 'a
  = fun f acc v_list -> match v_list with
    | VIdent (x, _) -> f acc x
    | VEmpty -> acc
    | VTuple (x, _, v_list) -> var_list_fold f (f acc x) v_list
                             
let rec var_list_map : type b. (Ast_parsing.ident -> 'a) -> b var_list -> 'a list =
  fun f v_list -> match v_list with
                  | VIdent (x, _) -> [f x]
                  | VEmpty -> []
                  | VTuple (x, _, xs) -> (f x)::(var_list_map f xs)
