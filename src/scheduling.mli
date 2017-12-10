module type Scheduling = sig
  exception Error of string
  val schedule : Ast_normalized.nfile -> string -> Ast_normalized.nfile
end

module Simple : Scheduling
module Stupid : Scheduling
