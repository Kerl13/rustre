module type E = sig
  val build_filename: string -> string
  val extract_to: Format.formatter -> Ast_object.file * string -> unit
end
