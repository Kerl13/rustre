open Ast_typed

val var_list_fold : ('a -> 'b var_ident -> 'a) -> 'a -> 'b var_list -> 'a
