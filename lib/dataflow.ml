open O
open Instr_types
include Dataflow_intf

module InstrToBlockTransfer
    (Block : Block)
    (Transfer : InstrTransfer with module Instr = Block.Instr) =
struct
  module Block = Block
  module Domain = Transfer.Domain
  module InstrTransfer = Transfer

  let transfer _label block ~other_facts ~current_fact =
    (* print_s [%message "on label" ~label:(label : Label.t)]; *)
    let new_fact =
      (match Transfer.direction with
       | Direction.Forward -> Block.fold_instrs_forward
       | Direction.Backward -> Block.fold_instrs_backward)
        block
        ~init:(Transfer.combine other_facts)
        ~f:(fun i d -> Transfer.transfer d i)
    in
    if Transfer.changed ~current_fact ~new_fact
    then
      (* print_s
         [%message
           "changed"
             ~current_fact:(current_fact : domain)
             ~new_fact:(new_fact : domain)]; *)
      Some new_fact
    else (* print_s [%message "didn't change"]; *)
      None
  ;;

  let empty = Transfer.empty
  let direction = Transfer.direction
end

module Make (Graph : Graph) = struct
  module Graph = Graph
  module Node = Graph.Node
  module Block = Graph.Block
  module NodeQueue = Hash_queue.Make (Node)

  module MakeAnalysis (Transfer : BlockTransfer with module Block = Graph.Block) = struct
    let run graph =
      let initial_facts = Node.Map.empty in
      let predecessors_of_label = Graph.predecessors_staged graph |> Staged.unstage in
      let queue = NodeQueue.create () in
      let _ =
        NodeQueue.enqueue_exn
          queue
          `back
          (match Transfer.direction with
           | Forward -> Graph.entry graph
           | Backward -> Graph.exit graph)
          ()
      in
      let rec go fact_base =
        match NodeQueue.dequeue_with_key queue `front with
        | None -> fact_base
        | Some (label, ()) ->
          let current_block = Graph.get_block graph label in
          let current_fact =
            Map.find fact_base label |> Option.value ~default:Transfer.empty
          in
          let other_labels =
            match Transfer.direction with
            | Forward ->
              (* Option.value with default because the start has no predecessors *)
              predecessors_of_label label
            | Backward -> Block.jumps (Graph.get_block graph label)
          in
          let other_facts =
            (* safe because we should have initialized empty fact for all labels *)
            List.map
              ~f:(fun node ->
                Map.find fact_base node |> Option.value ~default:Transfer.empty)
              other_labels
          in
          let maybe_new_facts =
            Transfer.transfer label current_block ~other_facts ~current_fact
          in
          (match maybe_new_facts with
           | Some new_fact ->
             (* the fact changed, so we need to add all labels that depend on the current label *)
             let labels_todo =
               match Transfer.direction with
               | Forward -> Block.jumps current_block
               | Backward -> predecessors_of_label label
             in
             List.iter labels_todo ~f:(fun label ->
               ignore (NodeQueue.enqueue queue `back label ()));
             let fact_base = Map.set fact_base ~key:label ~data:new_fact in
             go fact_base
           | None -> go fact_base)
      in
      go initial_facts
    ;;
  end
end

module Liveness = struct
  module Make (Instr : InstrWithValue) = struct
    module Domain = Set.Make_using_comparator (Instr.Value)
    module Instr = Instr

    let transfer instr prev_facts =
      let new_facts =
        prev_facts
        |> Fn.flip Set.diff (Domain.of_list (Instr.defs instr))
        |> Set.union (Domain.of_list (Instr.uses instr))
      in
      (* print_s
         [%message
         "transfer"
           ~instr:(instr : Instr.t)
           ~prev_facts:(prev_facts : domain)
           ~new_facts:(new_facts : domain)]; *)
      new_facts
    ;;

    let direction = Direction.Backward
    let empty = Domain.empty
    let changed ~current_fact ~new_fact = Set.length new_fact > Set.length current_fact
    let combine = List.fold_left ~init:Domain.empty ~f:Set.union
  end
end

module Dominators = struct
  module Make (Block : Block) = struct
    module Node = Block.Node
    module Block = Block
    module Domain = Block.Node.Set

    (* normally you use the set of all labels, but we use the empty set to make the sets smaller *)
    (* this means that we have to make sure that we aren't doing the intersection of some set with the empty set *)
    (* because the empty set isn't the bottom/top for set intersections, its the zero element *)
    let empty = Domain.empty
    let direction = Direction.Forward

    let transfer label _block ~other_facts ~current_fact =
      let new_fact =
        other_facts
        |> List.filter ~f:(fun s -> Set.length s > 0)
        |> List1.of_list
        |> Option.map ~f:(List1.fold_map ~f:Fn.id ~combine:Set.inter)
        |> Option.value ~default:Domain.empty
        |> Fn.flip Set.add label
      in
      Option.some_if (Set.length new_fact > Set.length current_fact) new_fact
    ;;
  end

  module MakeHelpers (Block : Block) = struct
    module Node = Block.Node

    (* the idom for start always points to start *)
    let compute_idoms_from_facts (start_label : Block.Node.t) (d : Node.Set.t Node.Map.t) =
      (* make it strictly dominates *)
      let d =
        Map.mapi
          ~f:(fun ~key:label ~data:dominates_label ->
            Set.filter dominates_label ~f:(fun label' ->
              not @@ [%equal: Node.t] label label'))
          d
      in
      (* d is label l -> labels l' s.t. l' > l *)
      (* inverted_facts is label l -> labels l' s.t. l > l' *)
      let inverted_facts =
        Data_graph.predecessors_of_map_graph
          ~empty:Node.Map.empty
          ~singleton:Node.Set.singleton
          d
      in
      (* print_s
         [%message
        "inverted facts" ~inverted_facts:(inverted_facts : Label.Set.t Label.Map.t)]; *)
      let idoms =
        Map.mapi d ~f:(fun ~key:label ~data:dominates_label ->
          let idom =
            Set.find dominates_label ~f:(fun label' ->
              let label'_dominates =
                Map.find inverted_facts label' |> Option.value ~default:Node.Set.empty
              in
              Set.inter dominates_label label'_dominates |> Set.is_empty)
          in
          match idom with
          | None ->
            assert ([%equal: Node.t] label start_label);
            start_label
          | Some idom -> idom)
      in
      idoms
    ;;

    let assert_is_tree start_label tree =
      let visited = Hash_set.create (module Node) in
      let rec go label =
        if Hash_set.mem visited label then raise_s [%message "the graph wasn't a tree"];
        Hash_set.add visited label;
        let children =
          Map.find tree label |> Option.value ~default:Node.Set.empty |> Set.to_list
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
                    if [%equal: Node.t] start_label label
                    then Node.Set.empty
                    else Node.Set.singleton label)))
          (F.Reduce.to_map_combine Node.Map.empty ~combine:Set.union)
          idoms
      in
      assert_is_tree start_label idom_tree;
      idom_tree
    ;;
  end
end
