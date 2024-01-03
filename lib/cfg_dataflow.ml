open O
open Instr_types

module type BlockLike = sig
  type t [@@deriving sexp_of]
  type instr

  val jumps : t -> Label.t list
  val fold_instrs_forward : init:'a -> f:('a -> instr -> 'a) -> t -> 'a
  val fold_instrs_backward : init:'a -> f:('a -> instr -> 'a) -> t -> 'a
end

module type InstrLike = sig
  type t [@@deriving sexp_of]

  module Value : sig
    type t [@@deriving equal, compare, sexp, hash]
  end

  val uses : t -> Value.t list
  val defs : t -> Value.t list
end

module Direction = struct
  type t = Forward | Backward
end

module type InstrTransfer = sig
  type instr [@@deriving sexp_of]
  type domain [@@deriving sexp_of]

  val transfer : instr -> domain -> domain
  val changed : domain -> domain -> bool
  val empty : domain
  val combine : domain list -> domain
  val direction : Direction.t
end

module type BlockTransfer = sig
  type block
  type domain [@@deriving sexp_of]

  val transfer : Label.t -> block -> domain list -> domain -> domain Option.t
  val empty : domain
  val direction : Direction.t
end

module type BlockRewriteTransfer = sig
  type block
  type domain [@@deriving sexp_of]
end

module InstrToBlockTransfer
    (Block : BlockLike)
    (Transfer : InstrTransfer with type instr = Block.instr) :
  BlockTransfer with type block = Block.t and type domain = Transfer.domain =
struct
  type block = Block.t
  type domain = Transfer.domain [@@deriving sexp_of]

  let transfer label block other_facts current_fact =
    (* let new_facts =  *)
    failwith ""

  let empty = Transfer.empty
  let direction = Transfer.direction
end

module MakeDataflowForBlock (Block : BlockLike) = struct
  module Graph = Cfg_graph.MakeGraph (Block)

  module MakeRun (Transfer : BlockTransfer with type block = Block.t) = struct
    let run (graph : Graph.t) : Transfer.domain Label.Map.t =
      let module Queue = Hash_queue.Make (Label) in
      let initial_facts =
        Label.Map.map ~f:(fun _ -> Transfer.empty) graph.body
      in
      let transpose =
        graph.body |> Map.to_alist
        |> List.bind ~f:(fun (parent_label, block) ->
               Block.jumps block
               |> List.map ~f:(fun jump_label -> (jump_label, parent_label)))
        |> Label.Map.of_alist_multi
      in
      let queue = Queue.create () in
      let _ =
        Queue.enqueue queue `back
          (match Transfer.direction with
          | Forward -> graph.entry
          | Backward -> graph.exit)
          ()
      in
      let rec go fact_base =
        match Queue.dequeue_with_key queue `front with
        | None -> fact_base
        | Some (label, ()) -> (
            let current_block = Map.find_exn graph.body label in
            let current_fact = Map.find_exn fact_base label in
            let other_labels =
              match Transfer.direction with
              | Forward -> Map.find transpose label |> Option.value ~default:[]
              | Backward -> Block.jumps (Map.find_exn graph.body label)
            in
            let other_facts =
              List.map ~f:(Map.find_exn fact_base) other_labels
            in
            let maybe_new_facts =
              Transfer.transfer label current_block other_facts current_fact
            in
            match maybe_new_facts with
            | Some new_fact -> go (Map.set fact_base ~key:label ~data:new_fact)
            | None -> go fact_base)
      in
      go initial_facts
  end
end

module MakeLivenessInstrTransfer (Instr : InstrLike) :
  InstrTransfer with type instr = Instr.t = struct
  module DomainSet = Set.Make (Instr.Value)

  type instr = Instr.t [@@deriving sexp_of]
  type domain = DomainSet.t [@@deriving sexp_of]

  let transfer instr prev_facts =
    prev_facts
    |> Set.diff (DomainSet.of_list (Instr.defs instr))
    |> Set.union (DomainSet.of_list (Instr.uses instr))

  let direction = Direction.Backward
  let empty = DomainSet.empty

  let changed current_fact new_fact =
    Set.length current_fact > Set.length new_fact

  let combine = List.fold_left ~init:DomainSet.empty ~f:Set.union
end

module MakeDominatorsBlockTransfer (Block : BlockLike) :
  BlockTransfer with type block = Block.t = struct
  type block = Block.t
  type domain = Label.Set.t [@@deriving sexp_of]

  let transfer _ = failwith ""
  let empty = Label.Set.empty
  let direction = Direction.Forward
end

module Make (Instr : InstrLike) (Block : BlockLike with type instr = Instr.t) =
struct
  include MakeDataflowForBlock (Block)

  module Dominators = struct
    module BlockTransfer = MakeDominatorsBlockTransfer (Block)
    include MakeRun (BlockTransfer)
  end
end
