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

let rec var_list_map2 : type b c. (Ast_parsing.ident -> Ast_parsing.ident -> 'a) -> b var_list -> c var_list -> 'a list =
  fun f l1 l2 -> match l1, l2 with
                 | VIdent (x1,_), VIdent (x2, _) -> [f x1 x2]
                 | VEmpty, VEmpty -> []
                 | VTuple (x1, _, x1s), VTuple (x2, _, x2s) -> (f x1 x2)::var_list_map2 f x1s x2s
                 | _, _ -> failwith "error in var_list_map2"

let rec var_list_fold2 : type b c. ('a -> Ast_parsing.ident -> Ast_parsing.ident -> 'a) -> 'a -> b var_list -> c var_list -> 'a
  = fun f acc v1 v2 -> match v1, v2 with
                       | VIdent (x1, _), VIdent (x2, _) -> f acc x1 x2
                       | VEmpty, VEmpty -> acc
                       | VTuple (x1, _, x1s), VTuple (x2, _, x2s) -> var_list_fold2 f (f acc x1 x2) x1s x2s
                       | _, _ -> failwith "error in var_list_fold2"
