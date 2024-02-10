open! O
open Utils.Instr_types
open Graph

(* module type Gen_S = sig
  type 'a block

  val to_graph : 'a block t -> Label.t Data.Graph.t
  val to_double_graph : 'a block t -> Label.t Data.Graph.double
  val predecessors_of_label : 'a block t -> Label.t list Label.Map.t
  val get_idoms : 'a block t -> Dominators.Idoms.t

  module Dfs : sig
    val reverse_postorder : 'a block t -> (Label.t, read_write) Vec.t
    val preorder : 'a block t -> (Label.t, read_write) Vec.t
  end
end

module type Block_gen = sig
  type 'a t

  val jumps_fold : (Label.t, 'a t) F.Fold.t
end

module Make_gen (Block : Block_gen) : Gen_S with type 'a block := 'a Block.t = struct
  let jumps b = Block.jumps_fold b
  let to_graph g = to_graph ~jumps g
  let to_double_graph g = to_double_graph ~jumps g
  let predecessors_of_label g = predecessors_of_label ~jumps g
  let get_idoms g = get_idoms ~jumps g

  module Dfs = struct
    let reverse_postorder g = Dfs.reverse_postorder ~jumps g
    let preorder g = Dfs.preorder ~jumps g
  end
end *)
