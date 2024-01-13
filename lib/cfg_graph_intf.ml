open O
open Instr_types

type 'b t =
  { entry : Label.t
  ; blocks : 'b Label.Map.t
  ; exit : Label.t
  }
[@@deriving sexp_of, fields]

module type Block = sig
  type t [@@deriving sexp_of]

  val jumps_fold : (Label.t, t) F.Fold.t
  val jumps : t -> Label.t list
end

module type S = sig
  val entry : 'b t -> Label.t
  val blocks : 'b t -> 'b Label.Map.t
  val exit : 'b t -> Label.t

  module Fields : module type of Fields

  val to_graph : jumps:('b -> Label.t F.Iter.t) -> 'b t -> Label.t Data_graph.t
  val map_blocks : 'b t -> f:('b Label.Map.t -> 'c Label.Map.t) -> 'c t
  val set_block : 'b t -> Label.t -> 'b -> 'b t
  val add_block_exn : 'b t -> Label.t -> 'b -> 'b t
  val validate : 'b t -> unit

  val predecessors_of_label
    :  jumps:('b -> Label.t list)
    -> 'b t
    -> Label.t list Label.Map.t

  val map_simple_order : 'b t -> f:(Label.t * 'b -> 'b) -> 'b t
end

module type Intf = sig
  type nonrec 'b t = 'b t [@@deriving sexp_of]

  include S
end
