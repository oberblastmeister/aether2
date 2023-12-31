open O
open Cfg_types

let label = Label.T "asdfasdf"

module type USEDEFS = sig
  type 'c i
  type t [@@deriving equal, compare, sexp, hash]

  val uses : 'c i -> t list
  val defs : 'c i -> t list
end

module MakeDataflow (I : Instr) = struct
  module Block = MakeBlock (I)

  module Graph = struct
    type t = Block.t Graph.t
  end

  module type BlockTransfer = sig
    type domain

    val transfer :
      Label.t -> Block.t -> domain list -> domain -> domain Option.t

    val empty : domain
    val direction : [ `Forward | `Backward ]
  end

  module type InstrTransfer = sig
    type domain

    val transfer : 'c I.t -> domain -> domain
    val changed : domain -> domain -> bool
    val empty : domain
    val combine : domain list -> domain
    val direction : [ `Forward | `Backward ]
  end

  module InstrToBlockTransfer (T : InstrTransfer) : BlockTransfer = struct
    type domain = T.domain

    let transfer label block other_facts current_fact =
      (* let new_facts =  *)
      failwith ""
    let empty = T.empty
    let direction = T.direction
  end

  module MakeRun (T : BlockTransfer) = struct
    let run (graph : Graph.t) : T.domain Label.Map.t =
      let module Queue = Hash_queue.Make (Label) in
      let initial_facts = Label.Map.map ~f:(fun _ -> T.empty) graph.body in
      let transpose =
        graph.body |> Map.to_alist
        |> List.bind ~f:(fun (parent_label, block) ->
               I.jumps block.exit
               |> List.map ~f:(fun jump_label -> (jump_label, parent_label)))
        |> Label.Map.of_alist_multi
      in
      let queue = Queue.create () in
      let _ =
        Queue.enqueue queue `back
          (match T.direction with
          | `Forward -> graph.entry
          | `Backward -> graph.exit)
          ()
      in
      let rec go fact_base =
        match Queue.dequeue_with_key queue `front with
        | None -> fact_base
        | Some (label, ()) -> (
            let current_block = Map.find_exn graph.body label in
            let current_fact = Map.find_exn fact_base label in
            let other_labels =
              match T.direction with
              | `Forward -> Map.find transpose label |> Option.value ~default:[]
              | `Backward -> I.jumps (Map.find_exn graph.body label).exit
            in
            let other_facts =
              List.map ~f:(Map.find_exn fact_base) other_labels
            in
            let maybe_new_facts =
              T.transfer label current_block other_facts current_fact
            in
            match maybe_new_facts with
            | Some new_fact -> go (Map.set fact_base ~key:label ~data:new_fact)
            | None -> go fact_base)
      in
      go initial_facts
  end
end

module MakeLiveness (I : Instr) (UseDefs : USEDEFS with type 'c i := 'c I.t) =
struct
  module Dataflow = MakeDataflow (I)
  module DomainSet = Set.Make (UseDefs)

  module InstrTransfer : Dataflow.InstrTransfer = struct
    type domain = DomainSet.t

    let transfer instr prev_facts =
      prev_facts
      |> Set.diff (DomainSet.of_list (UseDefs.defs instr))
      |> Set.union (DomainSet.of_list (UseDefs.uses instr))

    let direction = `Backward
    let empty = DomainSet.empty

    let changed current_fact new_fact =
      Set.length current_fact > Set.length new_fact

    let combine = List.fold_left ~init:DomainSet.empty ~f:Set.union
  end

  module BlockTransfer = Dataflow.InstrToBlockTransfer (InstrTransfer)
end
