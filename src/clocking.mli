module type Clocking = sig
  exception Error of Ast_typed.location * string
  val clock_file: Ast_typed.file -> string -> Ast_clocked.file
  (** We must know which is the main node *)
end


module W : Clocking
module Stupid : Clocking
