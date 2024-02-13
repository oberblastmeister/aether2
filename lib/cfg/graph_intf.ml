open! O
open Utils.Instr_types

module type Block_gen = sig
  type 'a t

  val jumps_fold : (Label.t, 'a t) F.Fold.t
end

module type Gen_S = sig
  type 'b t
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

module type Intf = sig
  type 'b t [@@deriving sexp]

  module type Gen_S = Gen_S with type 'b t := 'b t

  val of_alist : entry:Label.t -> exit:Label.t -> (Label.t * 'b) list -> 'b t
  val to_alist : 'b t -> (Label.t * 'b) list
  val entry : 'b t -> Label.t
  val blocks : 'b t -> 'b Label.Map.t
  val exit : 'b t -> Label.t
  val to_graph : jumps:('b -> Label.t F.Iter.t) -> 'b t -> Label.t Data.Graph.t

  val to_double_graph
    :  jumps:('b -> Label.t F.Iter.t)
    -> 'b t
    -> Label.t Data.Graph.double

  val find_exn : Label.t -> 'b t -> 'b
  val set : Label.t -> 'b -> 'b t -> 'b t
  val set_blocks_alist : (Label.t * 'b) list -> _ t -> 'b t
  val foldi : 'b t -> init:'a -> f:('a -> Label.t * 'b -> 'a) -> 'a
  val iter_labels : 'b t -> Label.t F.Iter.t
  val to_iteri : 'b t -> (Label.t * 'b) F.Iter.t
  val fold : 'b t -> init:'a -> f:('a -> 'b -> 'a) -> 'a
  val to_iter : 'b t -> 'b F.Iter.t
  val add_exn : 'b t -> Label.t -> 'b -> 'b t
  val validate : 'b t -> unit

  val predecessors_of_label
    :  jumps:('b -> Label.t F.Iter.t)
    -> 'b t
    -> Label.t list Label.Map.t

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val mapi : 'a t -> f:(Label.t * 'a -> 'b) -> 'b t
  val map_simple_order : 'b t -> f:(Label.t * 'b -> 'b) -> 'b t
  val fold_labels : (Label.t, [> read ]) Vec.t -> (Label.t * 'b, 'b t) F.Fold.t

  module Dfs : sig
    val reverse_postorder
      :  jumps:('b -> Label.t F.Iter.t)
      -> 'b t
      -> (Label.t, Perms.Read_write.t) Vec.t
  end

  module Make_gen : functor (Block : Block_gen) -> Gen_S with type 'a block := 'a Block.t
end
