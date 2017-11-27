open Ast_normalized

module type Scheduling = sig
  val schedule : Ast_normalized.nfile -> string -> Ast_normalized.nfile
end

module Graph = struct
  module Vertex = struct
    type t = string
    let compare (s1 : t) (s2 : t) = Pervasives.compare s1 s2
  end
  module Vmap = Map.Make(Vertex)
  module Vset = Set.Make(Vertex)

  type t = Vset.t Vmap.t

  let empty = Vmap.empty
  let add_vertex (g : t) x = Vmap.add x Vset.empty g
end

module Simple = struct
  exception Not_a_node of string

  let get_constraint _g _eq = assert false
  let reorder_eqs _g _eqs = assert false

  let schedule_node scheduled (NNode node) =
    let g = Ast_typed_utils.var_list_fold Graph.add_vertex Graph.empty node.n_output in
    let g = List.fold_left get_constraint g node.n_eqs in
    let eqs = reorder_eqs g node.n_eqs in
    let node = NNode { node with n_eqs = eqs } in
    node :: scheduled

  let rec cut_at acc name = function
    | [] -> raise (Not_a_node name)
    | NNode n :: nodes ->
      let Ast_typed.Tagged (_, _, name') = n.n_name in
      if name' = name
      then NNode n :: acc
      else cut_at (NNode n :: acc) name nodes
  let cut_at name = cut_at [] name

  let schedule file main =
    let nodes = cut_at main file.nf_nodes in
    let scheduled = List.fold_left schedule_node [] nodes in
    { file with nf_nodes = scheduled }
end

module Stupid = struct
  let schedule file _name = file
end
