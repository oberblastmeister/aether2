open! O
open Utils.Instr_types
module LabelQueue = Hash_queue.Make (Label)

type direction =
  | Forward
  | Backward

module type Value = sig
  type t [@@deriving equal, compare, hash, sexp_of]

  include Comparator.S with type t := t
end

module Graph = struct
  type 'b t =
    { entry : Label.t
    ; v : Label.t Data.Graph.double
    ; exit : Label.t
    ; get_block : Label.t -> 'b
    }

  let of_cfg ~jumps (graph : _ Graph.t) =
    { entry = Graph.entry graph
    ; v = Graph.to_double_graph ~jumps graph
    ; exit = Graph.exit graph
    ; get_block = (fun label -> Graph.find_exn label graph)
    }
  ;;
end

module Instr_transfer = struct
  type ('i, 'd) t =
    { transfer : 'i -> 'd -> 'd
    ; changed : current_fact:'d -> new_fact:'d -> bool
    ; empty : 'd
    ; combine : 'd list -> 'd
    ; direction : direction
    ; sexp_of_instr : 'i -> Sexp.t
    ; sexp_of_domain : 'd -> Sexp.t
    }

  let create
    ?(sexp_of_instr = sexp_of_opaque)
    ?(sexp_of_domain = sexp_of_opaque)
    ~transfer
    ~changed
    ~empty
    ~combine
    ~direction
    =
    { transfer; changed; empty; combine; direction; sexp_of_instr; sexp_of_domain }
  ;;

  let transfer t = t.transfer
end

module Block_transfer = struct
  type ('b, 'd) t =
    { transfer : Label.t -> 'b -> other_facts:'d -> current_fact:'d -> 'd option
    ; combine : 'd list -> 'd
    ; empty : 'd
    ; direction : direction
    ; sexp_of_domain : 'd -> Sexp.t
    ; sexp_of_block : 'b -> Sexp.t
    }

  let create
    ?(sexp_of_domain = sexp_of_opaque)
    ?(sexp_of_block = sexp_of_opaque)
    ~transfer
    ~combine
    ~empty
    ~direction
    =
    { transfer; combine; empty; direction; sexp_of_domain; sexp_of_block }
  ;;
end

module Fact_base = struct
  type 'd t = (Label.t, 'd) Entity.Map.t [@@deriving sexp_of]

  let find_exn = Label.Table.find_exn
end

let instr_to_block_transfer
  ?(sexp_of_block = sexp_of_opaque)
  ~iter_instrs_forward
  ~iter_instrs_backward
  (instr_transfer : _ Instr_transfer.t)
  : _ Block_transfer.t
  =
  let transfer _label block ~other_facts ~current_fact =
    let new_fact =
      F.Fold.fold
        (match instr_transfer.direction with
         | Forward -> iter_instrs_forward
         | Backward -> iter_instrs_backward)
        block
        ~init:other_facts
        ~f:(fun i d -> instr_transfer.transfer d i)
    in
    if instr_transfer.changed ~current_fact ~new_fact then Some new_fact else None
  in
  { transfer
  ; combine = instr_transfer.combine
  ; empty = instr_transfer.empty
  ; direction = instr_transfer.direction
  ; sexp_of_domain = instr_transfer.sexp_of_domain
  ; sexp_of_block
  }
;;

(* we have to run a full round first so we actually fill in all the liveness facts *)
(* then we can't decide to stop if something hasn't changed *)
(* algorihtm comes from engineering a compiler *)
let run_block_transfer (transfer : _ Block_transfer.t) (graph : _ Graph.t) =
  let fact_base = Label.Table.create () in
  let other_facts_base = Label.Table.create () in
  let labels =
    (match transfer.direction with
     | Forward -> Data.Graph.Dfs.reverse_postorder
     | Backward ->
       Data.Graph.Dfs.postorder (* TODO: might want to do rpo on the transpose *))
      ~start:[ graph.entry ]
      ~set:(Data.Constructors.some_hashset (module Label))
      (Data.Graph.t_of_double graph.v)
  in
  let changed = ref true in
  while !changed do
    changed := false;
    Vec.iter labels ~f:(fun label ->
      (* make sure that we're initializing all the facts *)
      if not (Label.Table.mem fact_base label)
      then Label.Table.set fact_base ~key:label ~data:transfer.empty;
      if not (Label.Table.mem other_facts_base label)
      then Label.Table.set other_facts_base ~key:label ~data:transfer.empty;
      [%log.global.debug (label : Label.t)];
      let current_block = graph.get_block label in
      let current_fact =
        Label.Table.find fact_base label |> Option.value ~default:transfer.empty
      in
      let other_labels =
        (match transfer.direction with
         | Forward ->
           (* Option.value with default because the start has no predecessors *)
           graph.v.preds
         | Backward -> graph.v.succs)
          label
      in
      let other_facts =
        F.Iter.map other_labels ~f:(fun node ->
          Label.Table.find fact_base node |> Option.value ~default:transfer.empty)
        |> F.Iter.to_list
        |> transfer.combine
      in
      Label.Table.set other_facts_base ~key:label ~data:other_facts;
      let maybe_new_facts =
        transfer.transfer label current_block ~other_facts ~current_fact
      in
      (match maybe_new_facts with
       | Some new_fact ->
         changed := true;
         Label.Table.set fact_base ~key:label ~data:new_fact
       | None -> ());
      ());
    ()
  done;
  fact_base, other_facts_base
;;

module Liveness = struct
  let make_transfer
    (type v cmp i)
    ?(sexp_of_instr = sexp_of_opaque)
    ~(value : (module Value with type t = v and type comparator_witness = cmp))
    ~uses
    ~defs
    : _ Instr_transfer.t
    =
    let module Value = (val value) in
    let module Domain = Set.Make_plain_using_comparator (Value) in
    let transfer instr prev_facts =
      let new_facts =
        prev_facts
        |> Fn.flip Set.diff (Set.of_list (module Value) (F.Iter.to_list (defs instr)))
        |> Set.union (Set.of_list (module Value) (F.Iter.to_list (uses instr)))
      in
      new_facts
    in
    { transfer
    ; direction = Backward
    ; empty = Set.empty (module Value)
    ; changed =
        (fun ~current_fact ~new_fact -> Set.length new_fact > Set.length current_fact)
    ; combine = List.fold_left ~init:(Set.empty (module Value)) ~f:Set.union
    ; sexp_of_instr
    ; sexp_of_domain = [%sexp_of: Domain.t]
    }
  ;;
end
