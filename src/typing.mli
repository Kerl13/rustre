exception Error of Ast_parsing.location * Ast_typed.ident

type typed_ty_wrapped = TypedTy: 'a Ast_typed.ty -> typed_ty_wrapped

val do_typing: Ast_parsing.file -> Ast_typed.file
val ty_to_typed_ty: Ast_parsing.ty -> typed_ty_wrapped
