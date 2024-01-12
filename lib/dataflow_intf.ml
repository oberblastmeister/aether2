open O
open Instr_types

module Types = struct
  type direction =
    | Forward
    | Backward

  module type Block = sig
    type t [@@deriving sexp_of]
  end

  module type Instr = sig
    type t [@@deriving sexp_of]
  end

  module type Domain = sig
    type t [@@deriving sexp_of]
  end

  module type Value = sig
    type t [@@deriving equal, compare, hash, sexp_of]

    include Comparator.S with type t := t
  end

  type 'i instr = (module Instr with type t = 'i)
  type 'b block = (module Block with type t = 'b)
  type 'd domain = (module Domain with type t = 'd)

  type ('i, 'd) instr_transfer =
    { transfer : 'i -> 'd -> 'd
    ; changed : current_fact:'d -> new_fact:'d -> bool
    ; empty : 'd
    ; combine : 'd list -> 'd
    ; direction : direction
    ; instr : 'i instr
    ; domain : 'd domain
    }

  type ('b, 'd) block_transfer =
    { transfer : Label.t -> 'b -> other_facts:'d list -> current_fact:'d -> 'd option
    ; empty : 'd
    ; direction : direction
    ; block : 'b block
    ; domain : 'd domain
    }

  type 'b graph =
    { entry : Label.t
    ; v : Label.t Graphs.double
    ; exit : Label.t
    ; get_block : Label.t -> 'b
    }

  type ('i, 'b) block_folds =
    { fold_instrs_forward : ('i, 'b) F.Fold.t
    ; fold_instrs_backward : ('i, 'b) F.Fold.t
    }

  type ('v, 'comparator_witness, 'i) liveness_dict =
    { value :
        (module Value with type t = 'v and type comparator_witness = 'comparator_witness)
    ; uses : ('v, 'i) F.Fold.t
    ; defs : ('v, 'i) F.Fold.t
    }
end

module type Intf = sig
  include module type of Types

  val instr_to_block_transfer
    :  'b block
    -> ('i, 'b) block_folds
    -> ('i, 'd) instr_transfer
    -> ('b, 'd) block_transfer

  val run_block_transfer : ('b, 'd) block_transfer -> 'b graph -> 'd Label.Map.t

  module Liveness : sig
    val make_transfer
      :  'i instr
      -> ('v, 'comparator_witness, 'i) liveness_dict
      -> ('i, ('v, 'comparator_witness) Set.t) instr_transfer
  end

  module Dominators : sig
    val make_transfer : 'b block -> ('b, Label.Set.t) block_transfer

    val compute_idoms_from_facts
      :  Label.t
      -> Label.Set.t Label.Map.t
      -> Label.t Label.Map.t

    val compute_idom_tree_from_facts
      :  Label.t
      -> Label.Set.t Label.Map.t
      -> Label.Set.t Label.Map.t
  end
end

include Types
