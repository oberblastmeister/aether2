open O
open Instr_types

module Common = struct
  module type S = sig
    type t

    include Sexpable.S with type t := t
    include Comparable.S with type t := t
    include Equal.S with type t := t
  end
end

module SomeInstr = struct
  type 'i t = T : ('c -> 'i) Higher_kinded.t -> 'i t
end

module type InstrBasic = sig
  type 'c t

  val sexp_of_t : 'c t -> Sexp.t
end

module type InstrBasic1 = sig
  type ('v, 'c) t

  val sexp_of_t : ('v -> Sexp.t) -> ('v, 'c) t -> Sexp.t
end

module type Instr = sig
  open Control

  type 'c t

  include InstrBasic with type 'c t := 'c t
  include Higher_kinded.S with type 'c t := 'c t

  module Some : sig
    type t = higher_kinded SomeInstr.t [@@deriving sexp_of]
  end

  module Value : sig
    type t [@@deriving equal, compare, sexp, hash]
  end

  (* returns Some if equal and proof *)
  val equal : 'c t -> 'd t -> bool
  val jumps : c t -> Label.t list
  val to_some : 'c t -> Some.t
  val uses : 'c t -> Value.t list
  val defs : 'c t -> Value.t list
end

(* module BlockType (I : InstrBasic) = struct
     type t = {
       entry : Control.e I.t list;
       body : Control.o I.t list;
       exit : Control.c I.t;
     }
     [@@deriving accessors]

     let sexp_of_t ({ entry; body; exit } : t) =
       [%sexp
         ("entry", (I.sexp_of_t entry : Sexp.t)),
           ("body", (List.map ~f:I.sexp_of_t body : Sexp.t list)),
           ("exit", (I.sexp_of_t exit : Sexp.t))]
   end

   module MakeTypeModules (I : InstrBasic) = struct
     module Block = BlockType (I)
     module Graph = Cfg_graph.MakeGraph (Block)
   end *)

(* create all the dataflow stuff that you need for an instruction set *)
module Make (Block : sig
  module Instr : Instr

  type t = {
    entry : Control.e Instr.t;
    body : Control.o Instr.t list;
    exit : Control.c Instr.t;
  }
  [@@deriving sexp_of]
end) =
struct
  module Instr = Block.Instr

  module DataflowBlock = struct
    type t = Block.t [@@deriving sexp_of]
    type instr = Instr.higher_kinded SomeInstr.t

    let jumps (block : t) = Instr.jumps block.exit

    let fold_instrs_forward ~init ~f (block : t) =
      let init = f init (Instr.to_some block.entry) in
      let init =
        List.fold_left ~init ~f:(fun z i -> f z (Instr.to_some i)) block.body
      in
      f init (Instr.to_some block.exit)

    let fold_instrs_backward ~init ~f (block : t) =
      let init = f init (Instr.to_some block.exit) in
      let init =
        List.rev block.body
        |> List.fold_left ~init ~f:(fun z i -> f z (Instr.to_some i))
      in
      f init (Instr.to_some block.entry)
  end

  module DataflowInstr = struct
    type t = Instr.Some.t [@@deriving sexp_of]

    module Value = Instr.Value

    let uses (SomeInstr.T i) = Instr.uses (Instr.project i)
    let defs (SomeInstr.T i) = Instr.defs (Instr.project i)
  end

  module Dataflow = Cfg_dataflow.MakeDataflowForBlock (DataflowBlock)

  module Liveness = struct
    module InstrTransfer = Cfg_dataflow.MakeLivenessInstrTransfer (DataflowInstr)

    module BlockTransfer =
      Cfg_dataflow.InstrToBlockTransfer (DataflowBlock) (InstrTransfer)

    include Dataflow.MakeRun (BlockTransfer)
  end

  module Dominators = struct
    module BlockTransfer =
      Cfg_dataflow.MakeDominatorsBlockTransfer (DataflowBlock)

    include Dataflow.MakeRun (BlockTransfer)
  end
end
