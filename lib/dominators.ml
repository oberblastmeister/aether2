open O

module MakeDominators (Graph : Data_graph.SingleEntryGraph) = struct
  module Idoms = struct
    type t = (Graph.Node.t, Graph.Node.t) Hashtbl.t
  end

  module Node = Graph.Node
  module NodeHashtbl = Hashtbl.Make (Graph.Node)
  module Node_hash_set = Hash_set.Make (Graph.Node)
  module Dfs = Data_graph.Dfs (Graph)

  let dominance_frontier_of_idoms predecessors idoms =
    let is_join_point preds =
      match preds with
      | _ :: _ :: _ -> true
      | _ -> false
    in
    let frontier_of_node = NodeHashtbl.create () in
    let rec add_until node node_idom runner =
      if not @@ [%equal: Node.t] runner node_idom
      then (
        (* add node to runner's frontier set because runner doesn't dominate node *)
        Hashtbl.update frontier_of_node runner ~f:(function
          | None -> Node_hash_set.create ()
          | Some fs ->
            Hash_set.add fs node;
            fs);
        add_until node node_idom (Hashtbl.find_exn idoms runner))
    in
    (* the idoms map should contain all the nodes in the graph *)
    Hashtbl.iter_keys idoms ~f:(fun node ->
      let preds = predecessors node in
      if is_join_point preds
      then
        List.iter preds ~f:(fun pred -> add_until node (Hashtbl.find_exn idoms node) pred));
    frontier_of_node
  ;;

  let get_idoms' predecessors (graph : Graph.t) : Idoms.t =
    let start_node = Graph.entry graph in
    let idom_of_node = NodeHashtbl.create () in
    (* special case for start node *)
    Hashtbl.set idom_of_node ~key:start_node ~data:start_node;
    let nodes = Dfs.postorder [ start_node ] graph in
    let index_of_node = NodeHashtbl.create ~size:(Vec.length nodes) () in
    Vec.iteri nodes ~f:(fun i node -> Hashtbl.set index_of_node ~key:node ~data:i);
    let intersect node1 node2 =
      (* print_s
         [%message "intersect" ~node1:(node1 : Graph.Node.t) ~node2:(node2 : Graph.Node.t)]; *)
      let with_index node = node, Hashtbl.find_exn index_of_node node in
      let go_up node = with_index (Hashtbl.find_exn idom_of_node node) in
      let rec go (node1, i1) (node2, i2) =
        (* the ones with higher indexes are closer to the top of the rpo tree *)
        match Ordering.of_int ([%compare: int] i1 i2) with
        | Less -> go (go_up node1) (node2, i2)
        | Greater -> go (node1, i1) (go_up node2)
        | Equal -> node1
      in
      go (with_index node1) (with_index node2)
    in
    let compute () =
      (* fold from the right for reverse post order *)
      Vec.fold_right nodes ~init:false ~f:(fun node changed ->
        if [%equal: Node.t] (Graph.entry graph) node
        then changed
        else (
          let preds = predecessors node in
          (* print_s [%message "compute" ~node:(node : Node.t) ~preds:(preds : Node.t list)]; *)
          let chosen_pred =
            preds
            |> List.find ~f:(Hashtbl.mem idom_of_node)
            |> Option.value_exn
                 ~message:
                   "must have idom because reverse post order guarantees that we have \
                    processed at least one predecessor"
          in
          let other_preds =
            preds |> List.filter ~f:(fun pred -> not @@ [%equal: Node.t] pred chosen_pred)
          in
          let new_idom =
            List.fold other_preds ~init:chosen_pred ~f:(fun current_idom pred ->
              match Hashtbl.find idom_of_node pred with
              | None -> current_idom
              | Some pred_idom -> intersect pred_idom current_idom)
          in
          (* print_s [%message "new_idom" (node : Node.t) (new_idom : Node.t)]; *)
          let changed =
            not
            @@ [%equal: Graph.Node.t option]
                 (Hashtbl.find idom_of_node node)
                 (Some new_idom)
          in
          Hashtbl.set idom_of_node ~key:node ~data:new_idom;
          changed))
    in
    let rec fixpoint () =
      match compute () with
      | true -> fixpoint ()
      | false -> ()
    in
    fixpoint ();
    idom_of_node
  ;;

  let get_idoms graph =
    get_idoms' (Graph.predecessors_staged graph |> Staged.unstage) graph
  ;;

  let idoms_and_frontier graph =
    let preds = Graph.predecessors_staged graph |> Staged.unstage in
    let idoms = get_idoms' preds graph in
    let frontier = dominance_frontier_of_idoms preds idoms in
    idoms, frontier
  ;;
end

let%test_module _ =
  (module struct
    module TestGraph = Data_graph.Make_map_graph_using_comparator (String)

    module Dominators = MakeDominators (struct
        include TestGraph

        let entry _ = "start"
      end)

    let set = String.Set.of_list

    (* Figure 2 *)
    let%expect_test _ =
      let g =
        String.Map.of_alist_exn
          [ "start", set [ "4"; "3" ]
          ; "4", set [ "1" ]
          ; "3", set [ "2" ]
          ; "2", set [ "1" ]
          ; "1", set [ "2" ]
          ]
      in
      let idoms = Dominators.get_idoms g in
      print_s [%sexp (idoms : (string, string) Hashtbl.t)];
      ();
      [%expect {| ((1 start) (2 start) (3 start) (4 start) (start start)) |}]
    ;;

    (* Figure 4 *)
    let%expect_test _ =
      let g =
        String.Map.of_alist_exn
          [ "start", set [ "5"; "4" ]
          ; "5", set [ "1" ]
          ; "4", set [ "2"; "3" ]
          ; "1", set [ "2" ]
          ; "2", set [ "1"; "3" ]
          ; "3", set [ "2" ]
          ]
      in
      let idoms = Dominators.get_idoms g in
      print_s [%sexp (idoms : (string, string) Hashtbl.t)];
      [%expect {| ((1 start) (2 start) (3 start) (4 start) (5 start) (start start)) |}]
    ;;
  end)
;;
