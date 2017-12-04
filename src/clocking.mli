module type Clocking = sig
  exception ClockingError of Ast_typed.location * string
  val clock_file: Ast_typed.file -> Ast_clocked.file
end


module W : Clocking
module Stupid : Clocking
