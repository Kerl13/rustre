exception Bad_type
type typed_ty_wrapped = TypedTy: 'a Ast_typed.ty -> typed_ty_wrapped
exception Expected_type of typed_ty_wrapped * typed_ty_wrapped
val do_typing: Ast_parsing.file -> Ast_typed.file
