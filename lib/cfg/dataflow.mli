open O
open Utils.Instr_types

type direction =
  | Forward
  | Backward

module type Value = sig
  type t [@@deriving equal, compare, hash, sexp_of]

  include Comparator.S with type t := t
end

module Graph : sig
  type 'b t

  val of_cfg : jumps:(Label.t, 'b) F.Fold.t -> 'b Graph.t -> 'b t
end

module Instr_transfer : sig
  type ('i, 'd) t

  val create
    :  ?sexp_of_instr:('i -> Sexp.t)
    -> ?sexp_of_domain:('d -> Sexp.t)
    -> transfer:('i -> 'd -> 'd)
    -> changed:(current_fact:'d -> new_fact:'d -> bool)
    -> empty:'d
    -> combine:('d list -> 'd)
    -> direction:direction
    -> ('i, 'd) t

  val transfer : ('i, 'd) t -> 'i -> 'd -> 'd
end

module Block_transfer : sig
  type ('b, 'd) t

  val create
    :  ?sexp_of_domain:('d -> Sexp.t)
    -> ?sexp_of_block:('b -> Sexp.t)
    -> transfer:(Label.t -> 'b -> other_facts:'d -> current_fact:'d -> 'd option)
    -> combine:('d list -> 'd)
    -> empty:'d
    -> direction:direction
    -> ('b, 'd) t
end

module Fact_base : sig
  type 'd t [@@deriving sexp_of]

  val find_exn : 'd t -> Label.t -> 'd
end

val instr_to_block_transfer
  :  ?sexp_of_block:('b -> Sexp.t)
  -> iter_instrs_forward:('i, 'b) F.Fold.t
  -> iter_instrs_backward:('i, 'b) F.Fold.t
  -> ('i, 'd) Instr_transfer.t
  -> ('b, 'd) Block_transfer.t

val run_block_transfer
  :  ('b, 'd) Block_transfer.t
  -> 'b Graph.t
  -> 'd Fact_base.t * 'd Fact_base.t

module Liveness : sig
  val make_transfer
    :  ?sexp_of_instr:('i -> Sexp.t)
    -> value:(module Value with type t = 'v and type comparator_witness = 'cmp)
    -> uses:('v, 'i) F.Fold.t
    -> defs:('v, 'i) F.Fold.t
    -> ('i, ('v, 'cmp) Set.t) Instr_transfer.t
end

module Dominators : sig
  val make_transfer : ?sexp_of_block:('b -> Sexp.t) -> ('b, Label.Set.t) Block_transfer.t
  (* val compute_idoms_from_facts : Label.t -> Label.Set.t Fact_base.t -> Label.t Label.Map.t

     val compute_idom_tree_from_facts
     :  Label.t
     -> Label.Set.t Fact_base.t
     -> Label.Set.t Fact_base.t *)
end
