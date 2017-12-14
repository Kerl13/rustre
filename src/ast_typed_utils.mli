open Ast_typed

val var_list_exists : (Ast_parsing.ident -> bool) -> 'a var_list -> bool
val var_list_for_all : (Ast_parsing.ident -> bool) -> 'a var_list -> bool
val var_list_fold : ('a -> Ast_parsing.ident -> 'a) -> 'a -> 'b var_list -> 'a
val var_list_map : (Ast_parsing.ident -> 'a) -> 'b var_list -> 'a list
val var_list_map2 : (Ast_parsing.ident -> Ast_parsing.ident -> 'a) -> 'b var_list -> 'c var_list -> 'a list
val var_list_fold2 : ('a -> Ast_parsing.ident -> Ast_parsing.ident -> 'a) -> 'a -> 'b var_list -> 'c var_list -> 'a
