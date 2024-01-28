open! O
include Dataflow_intf
open Utils.Instr_types
module LabelQueue = Hash_queue.Make (Label)

module Graph = struct
  type 'b t =
    { entry : Label.t
    ; v : Label.t Data.Graph.double
    ; exit : Label.t
    ; get_block : Label.t -> 'b
    }

  let of_cfg ~jumps (graph : _ Graph.t) =
    { entry = graph.entry
    ; v = Graph.to_double_graph ~jumps graph
    ; exit = graph.exit
    ; get_block = Map.find_exn graph.blocks
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
    ~transfer
    ~changed
    ~empty
    ~combine
    ~direction
    ?(sexp_of_instr = sexp_of_opaque)
    ?(sexp_of_domain = sexp_of_opaque)
    =
    { transfer; changed; empty; combine; direction; sexp_of_instr; sexp_of_domain }
  ;;

  let transfer t = t.transfer
end

module Block_transfer = struct
  type ('b, 'd) t =
    { transfer : Label.t -> 'b -> other_facts:'d list -> current_fact:'d -> 'd option
    ; empty : 'd
    ; direction : direction
    ; sexp_of_domain : 'd -> Sexp.t
    ; sexp_of_block : 'b -> Sexp.t
    }

  let create
    ~transfer
    ~empty
    ~direction
    ?(sexp_of_domain = sexp_of_opaque)
    ?(sexp_of_block = sexp_of_opaque)
    =
    { transfer; empty; direction; sexp_of_domain; sexp_of_block }
  ;;
end

let instr_to_block_transfer
  ?(sexp_of_block = sexp_of_opaque)
  block_folds
  (instr_transfer : _ Instr_transfer.t)
  : _ Block_transfer.t
  =
  let transfer _label block ~other_facts ~current_fact =
    let new_fact =
      F.Fold.fold
        (match instr_transfer.direction with
         | Forward -> block_folds.instrs_forward_fold
         | Backward -> block_folds.instrs_backward_fold)
        block
        ~init:(instr_transfer.combine other_facts)
        ~f:(fun i d -> instr_transfer.transfer d i)
    in
    if instr_transfer.changed ~current_fact ~new_fact then Some new_fact else None
  in
  { transfer
  ; empty = instr_transfer.empty
  ; direction = instr_transfer.direction
  ; sexp_of_domain = instr_transfer.sexp_of_domain
  ; sexp_of_block
  }
;;

let run_block_transfer (transfer : _ Block_transfer.t) (graph : _ Graph.t) =
  let initial_facts =
    F.Iter.fold graph.v.all_nodes ~init:Label.Map.empty ~f:(fun initial_facts label ->
      Map.set initial_facts ~key:label ~data:transfer.empty)
  in
  let queue = LabelQueue.create () in
  let _ =
    LabelQueue.enqueue_exn
      queue
      `back
      (match transfer.direction with
       | Forward -> graph.entry
       | Backward -> graph.exit)
      ()
  in
  let rec go fact_base =
    match LabelQueue.dequeue_with_key queue `front with
    | None -> fact_base
    | Some (label, ()) ->
      let current_block = graph.get_block label in
      (* we should have initialized all facts *)
      let current_fact = Map.find fact_base label |> Option.value_exn in
      let other_labels =
        (match transfer.direction with
         | Forward ->
           (* Option.value with default because the start has no predecessors *)
           graph.v.preds
         | Backward -> graph.v.succs)
          label
      in
      (* we should have initialized all facts *)
      let other_facts =
        F.Iter.map other_labels ~f:(fun node ->
          Map.find fact_base node |> Option.value_exn)
        |> F.Iter.to_list
      in
      let maybe_new_facts =
        transfer.transfer label current_block ~other_facts ~current_fact
      in
      (match maybe_new_facts with
       | Some new_fact ->
         (* the fact changed, so we need to add all labels that depend on the current label *)
         let labels_todo =
           match transfer.direction with
           | Forward -> graph.v.succs label
           | Backward -> graph.v.preds label
         in
         F.Iter.iter labels_todo ~f:(fun label ->
           ignore (LabelQueue.enqueue queue `back label ()));
         let fact_base = Map.set fact_base ~key:label ~data:new_fact in
         go fact_base
       | None -> go fact_base)
  in
  go initial_facts
;;

module Liveness = struct
  let make_transfer
    (type v cmp i)
    ?(sexp_of_instr = sexp_of_opaque)
    (dict : (v, cmp, i) liveness_dict)
    : _ Instr_transfer.t
    =
    let module Value = (val dict.value) in
    let module Domain = Set.Make_plain_using_comparator (Value) in
    let transfer instr prev_facts =
      let new_facts =
        prev_facts
        |> Fn.flip
             Set.diff
             (Set.of_list (module Value) (F.Iter.to_list (dict.defs instr)))
        |> Set.union (Set.of_list (module Value) (F.Iter.to_list (dict.uses instr)))
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

module Dominators = struct
  let make_transfer ?(sexp_of_block = sexp_of_opaque) : _ Block_transfer.t =
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
    in
    { direction = Forward
    ; empty = Label.Set.empty
    ; transfer
    ; sexp_of_domain = [%sexp_of: Label.Set.t]
    ; sexp_of_block
    }
  ;;

  (* the idom for start always points to start *)
  let compute_idoms_from_facts (start_label : Label.t) (d : Label.Set.t Label.Map.t) =
    (* make it strictly dominates *)
    let d =
      Map.mapi
        ~f:(fun ~key:label ~data:dominates_label ->
          Set.filter dominates_label ~f:(fun label' ->
            not @@ [%equal: Label.t] label label'))
        d
    in
    (* d is label l -> labels l' s.t. l' > l *)
    (* inverted_facts is label l -> labels l' s.t. l > l' *)
    let inverted_facts =
      F.Fold.fold
        (FC.Map.foldi @> F.Fold.ix FC.Set.fold @> F.Fold.of_fn (fun (x, y) -> y, x))
        d
        ~init:Label.Map.empty
        ~f:(fun inverted_facts (label, label_dominates) ->
          Map.update
            inverted_facts
            label
            ~f:
              (Option.value_map
                 ~default:(Label.Set.singleton label_dominates)
                 ~f:(Fn.flip Set.add label_dominates)))
    in
    let idoms =
      Map.mapi d ~f:(fun ~key:label ~data:dominates_label ->
        let idom =
          Set.find dominates_label ~f:(fun label' ->
            let label'_dominates =
              Map.find inverted_facts label' |> Option.value ~default:Label.Set.empty
            in
            Set.inter dominates_label label'_dominates |> Set.is_empty)
        in
        match idom with
        | None ->
          assert ([%equal: Label.t] label start_label);
          start_label
        | Some idom -> idom)
    in
    idoms
  ;;

  let assert_is_tree start_label tree =
    let visited = Hash_set.create (module Label) in
    let rec go label =
      if Hash_set.mem visited label then raise_s [%message "the graph wasn't a tree"];
      Hash_set.add visited label;
      let children =
        Map.find tree label |> Option.value ~default:Label.Set.empty |> Set.to_list
      in
      List.iter children ~f:(fun label -> go label)
    in
    go start_label
  ;;

  let compute_idom_tree_from_facts start_label d =
    let idoms = compute_idoms_from_facts start_label d in
    let idom_tree =
      F.Fold.reduce
        F.Fold.(
          F.Core.Map.foldi
          @> of_fn (fun (label, dominates_label) -> dominates_label, label)
          @> ix
               (of_fn (fun label ->
                  (* don't add the start label to the idom tree *)
                  if [%equal: Label.t] start_label label
                  then Label.Set.empty
                  else Label.Set.singleton label)))
        (F.Reduce.to_map_combine Label.Map.empty ~combine:Set.union)
        idoms
    in
    assert_is_tree start_label idom_tree;
    idom_tree
  ;;
end
