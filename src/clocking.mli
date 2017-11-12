module type Clocking = sig
  val clock_file: Ast_typed.file -> Ast_clocked.file
end

module Stupid : Clocking
