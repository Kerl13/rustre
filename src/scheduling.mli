module type Scheduling = sig
  exception Error of Ast_typed.location * string
  val schedule : Ast_normalized.nfile -> string -> Ast_normalized.nfile
end

module Simple : Scheduling
module Stupid : Scheduling
