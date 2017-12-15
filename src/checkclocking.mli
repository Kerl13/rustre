module type Checkclocking = sig
  exception Error of Ast_typed.location * string
  val check_clock_file: Ast_typed.file -> string -> Ast_clocked.file -> unit
  (** We must know which is the main node *)
end

module CheckW : Checkclocking
module Stupid : Checkclocking
