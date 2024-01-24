open! O
open Utils.Instr_types

type 'b t =
  { entry : Label.t
  ; blocks : 'b Label.Map.t
  ; exit : Label.t
  }
[@@deriving sexp, fields]

module type Block_gen = sig
  type 'a t

  val jumps_fold : (Label.t, 'a t) F.Fold.t
end

module type Gen_S = sig
  type 'a block

  val to_graph : 'a block t -> Label.t Data.Graph.t
  val to_double_graph : 'a block t -> Label.t Data.Graph.double
  val predecessors_of_label : 'a block t -> Label.t list Label.Map.t
  val get_idoms : 'a block t -> Dominators.Idoms.t

  module Dfs : sig
    val reverse_postorder : 'a block t -> (Label.t, Perms.Read_write.t) Vec.t
  end
end

module type Intf = sig
  type nonrec 'b t = 'b t [@@deriving sexp]

  val entry : 'b t -> Label.t
  val blocks : 'b t -> 'b Label.Map.t
  val exit : 'b t -> Label.t

  module Fields : module type of Fields

  val to_graph : jumps:('b -> Label.t F.Iter.t) -> 'b t -> Label.t Data.Graph.t

  val to_double_graph
    :  jumps:('b -> Label.t F.Iter.t)
    -> 'b t
    -> Label.t Data.Graph.double

  val map_blocks : 'b t -> f:('b Label.Map.t -> 'c Label.Map.t) -> 'c t
  val set_block : 'b t -> Label.t -> 'b -> 'b t
  val add_block_exn : 'b t -> Label.t -> 'b -> 'b t
  val validate : 'b t -> unit

  val predecessors_of_label
    :  jumps:('b -> Label.t F.Iter.t)
    -> 'b t
    -> Label.t list Label.Map.t

  val map_simple_order : 'b t -> f:(Label.t * 'b -> 'b) -> 'b t

  module Dfs : sig
    val reverse_postorder
      :  jumps:('b -> Label.t F.Iter.t)
      -> 'b t
      -> (Label.t, Perms.Read_write.t) Vec.t
  end

  module type Gen_S = Gen_S

  module Make_gen : functor (Block : Block_gen) -> Gen_S with type 'a block := 'a Block.t
end
