type local_variables = ((string * Ast_object.var_list) list)

module type E = sig
  val build_filename: string -> string
  val extract_to: Format.formatter -> Ast_object.file * string * local_variables -> unit
end
