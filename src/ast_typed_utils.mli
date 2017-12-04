open Ast_typed

val var_list_fold : ('a -> Ast_parsing.ident -> 'a) -> 'a -> 'b var_list -> 'a
