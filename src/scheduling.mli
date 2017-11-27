module type Scheduling = sig
  val schedule : Ast_normalized.nfile -> string -> Ast_normalized.nfile
end

module Simple : Scheduling
module Stupid : Scheduling
