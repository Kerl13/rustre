exception Bad_type
exception Expected_num of Ast_parsing.location
exception Type_error_at of Ast_parsing.location
type typed_ty_wrapped = TypedTy: 'a Ast_typed.ty -> typed_ty_wrapped
exception Expected_type of typed_ty_wrapped * typed_ty_wrapped * Ast_parsing.location
val do_typing: Ast_parsing.file -> Ast_typed.file
