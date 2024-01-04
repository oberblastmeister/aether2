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
  val changed : current_fact:domain -> new_fact:domain -> bool
  val empty : domain
  val combine : domain list -> domain
  val direction : Direction.t
end

module type BlockTransfer = sig
  type block
  type domain [@@deriving sexp_of]

  val transfer :
    Label.t ->
    block ->
    other_facts:domain list ->
    current_fact:domain ->
    domain Option.t

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

  let transfer label block ~other_facts ~current_fact =
    (* print_s [%message "on label" ~label:(label : Label.t)]; *)
    let new_fact =
      (match Transfer.direction with
      | Direction.Forward -> Block.fold_instrs_forward
      | Direction.Backward -> Block.fold_instrs_backward)
        ~init:(Transfer.combine other_facts)
        ~f:(Fn.flip Transfer.transfer)
        block
    in
    if Transfer.changed ~current_fact ~new_fact then
      (* print_s
         [%message
           "changed"
             ~current_fact:(current_fact : domain)
             ~new_fact:(new_fact : domain)]; *)
      Some new_fact
    else (* print_s [%message "didn't change"]; *)
      None

  let empty = Transfer.empty
  let direction = Transfer.direction
end

module LabelQueue = Hash_queue.Make (Label)

module Dataflow = struct
  module type S = sig end
end

module MakeDataflowForBlock (Block : BlockLike) = struct
  (* module Graph = Cfg_graph.MakeGraph (Block) *)
  module Graph = struct
    type t = Block.t Cfg_graph.Graph.t
  end

  module MakeRun (Transfer : BlockTransfer with type block = Block.t) = struct
    let run (graph : Graph.t) : Transfer.domain Label.Map.t =
      let initial_facts =
        Label.Map.map ~f:(fun _ -> Transfer.empty) graph.blocks
      in
      let transpose =
        graph.blocks |> Map.to_alist
        |> List.bind ~f:(fun (jumped_from, block) ->
               Block.jumps block
               |> List.map ~f:(fun jumped_to -> (jumped_to, jumped_from)))
        |> Label.Map.of_alist_multi
      in
      let queue = LabelQueue.create () in
      let _ =
        LabelQueue.enqueue_exn queue `back
          (match Transfer.direction with
          | Forward -> graph.entry
          | Backward -> graph.exit)
          ()
      in
      let rec go fact_base =
        match LabelQueue.dequeue_with_key queue `front with
        | None -> fact_base
        | Some (label, ()) -> (
            let current_block = Map.find_exn graph.blocks label in
            let current_fact = Map.find_exn fact_base label in
            let other_labels =
              match Transfer.direction with
              | Forward ->
                  (* Option.value with default because the start has no predecessors *)
                  Map.find transpose label |> Option.value ~default:[]
              | Backward -> Block.jumps (Map.find_exn graph.blocks label)
            in
            let other_facts =
              (* safe because we should have initialized empty fact for all labels *)
              List.map ~f:(Map.find_exn fact_base) other_labels
            in
            let maybe_new_facts =
              Transfer.transfer label current_block ~other_facts ~current_fact
            in
            match maybe_new_facts with
            | Some new_fact ->
                (* the fact changed, so we need to add all labels that depend on the current label *)
                let labels_todo =
                  match Transfer.direction with
                  | Forward -> Block.jumps current_block
                  | Backward ->
                      Map.find transpose label |> Option.value ~default:[]
                in
                List.iter labels_todo ~f:(fun label ->
                    ignore (LabelQueue.enqueue queue `back label ()));
                let fact_base = Map.set fact_base ~key:label ~data:new_fact in
                go fact_base
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
    let new_facts =
      prev_facts
      |> Fn.flip Set.diff (DomainSet.of_list (Instr.defs instr))
      |> Set.union (DomainSet.of_list (Instr.uses instr))
    in
    (* print_s
       [%message
         "transfer"
           ~instr:(instr : Instr.t)
           ~prev_facts:(prev_facts : domain)
           ~new_facts:(new_facts : domain)]; *)
    new_facts

  let direction = Direction.Backward
  let empty = DomainSet.empty

  let changed ~current_fact ~new_fact =
    Set.length new_fact > Set.length current_fact

  let combine = List.fold_left ~init:DomainSet.empty ~f:Set.union
end

module MakeDominatorsBlockTransfer (Block : BlockLike) :
  BlockTransfer with type block = Block.t = struct
  type block = Block.t
  type domain = Label.Set.t [@@deriving sexp_of]

  (* normally you use the set of all labels, but we use the empty set to make the sets smaller *)
  (* this means that we have to make sure that we aren't doing the intersection of some set with the empty set *)
  (* because the empty set isn't the bottom/top for set intersections, its the zero element *)
  let empty = Label.Set.empty
  let direction = Direction.Forward

  let transfer label _block ~other_facts ~current_fact =
    let new_fact =
      other_facts
      |> List.filter ~f:(fun s -> Set.length s > 0)
      |> List1.of_list
      |> Option.map ~f:(List1.fold_map ~f:Fn.id ~combine:Set.inter)
      |> Option.value ~default:Label.Set.empty
      |> Fn.flip Set.add label
    in
    Option.some_if (Set.length new_fact > Set.length current_fact) new_fact
end

module Make (Instr : InstrLike) (Block : BlockLike with type instr = Instr.t) =
struct
  include MakeDataflowForBlock (Block)

  module Dominators = struct
    module BlockTransfer = MakeDominatorsBlockTransfer (Block)
    include MakeRun (BlockTransfer)
  end
end
