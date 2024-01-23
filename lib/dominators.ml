open! O
open Utils.Instr_types

type 'n idoms = ('n, 'n) Hashtbl.t

let get_idoms start_node (graph : Label.t Data_graph.double) =
  let idom_of_node = Hashtbl.create (module Label) in
  (* special case for start node *)
  Hashtbl.set idom_of_node ~key:start_node ~data:start_node;
  let nodes =
    Data_graph.Dfs.postorder
      ~start:[ start_node ]
      ~set:(Constructors.some_hashset (module Label))
      (Data_graph.t_of_double graph)
  in
  let index_of_node = Hashtbl.create ~size:(Vec.length nodes) (module Label) in
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
      if [%equal: Label.t] start_node node
      then changed
      else (
        let preds = F.Fold.to_list graph.preds node in
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
          preds |> List.filter ~f:(fun pred -> not @@ [%equal: Label.t] pred chosen_pred)
        in
        let new_idom =
          List.fold other_preds ~init:chosen_pred ~f:(fun current_idom pred ->
            match Hashtbl.find idom_of_node pred with
            | None -> current_idom
            | Some pred_idom -> intersect pred_idom current_idom)
        in
        (* print_s [%message "new_idom" (node : Node.t) (new_idom : Node.t)]; *)
        let changed =
          not @@ [%equal: Label.t option] (Hashtbl.find idom_of_node node) (Some new_idom)
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

let frontier_of_idoms idoms (graph : Label.t Data_graph.double) =
  let is_join_point preds_length = preds_length >= 2 in
  let frontier_of_node = Hashtbl.create (module Label) in
  let rec add_until node node_idom runner =
    if not @@ [%equal: Label.t] runner node_idom
    then (
      (* add node to runner's frontier set because runner doesn't dominate node *)
      Hashtbl.update frontier_of_node runner ~f:(function
        | None -> Hash_set.create (module Label)
        | Some fs ->
          Hash_set.add fs node;
          fs);
      add_until node node_idom (Hashtbl.find_exn idoms runner))
  in
  (* the idoms map should contain all the nodes in the graph *)
  Hashtbl.iter_keys idoms ~f:(fun node ->
    let preds = graph.preds node |> Iter.length in
    if is_join_point preds
    then
      F.Iter.iter (graph.preds node) ~f:(fun pred ->
        add_until node (Hashtbl.find_exn idoms node) pred));
  frontier_of_node
;;

let%test_module _ =
  (module struct
    let set = String.Set.of_list
    let tbl = Hashtbl.create (module String)

    let lab s =
      match Hashtbl.find tbl s with
      | None ->
        let label = Label.of_string_global_unique s in
        Hashtbl.set tbl ~key:s ~data:label;
        label
      | Some l -> l
    ;;

    let graph xs =
      xs
      |> List.map ~f:(fun (n, ns) -> lab n, List.map ~f:lab ns)
      |> Label.Map.of_alist_exn
      |> Data_graph.t_of_map_list
      |> Data_graph.double_of_t (Constructors.some_hashtbl (module Label))
    ;;

    let get_idoms = get_idoms (lab "start")

    (* Figure 2 *)
    let%expect_test _ =
      let g =
        graph
          [ "start", [ "4"; "3" ]
          ; "4", [ "1" ]
          ; "3", [ "2" ]
          ; "2", [ "1" ]
          ; "1", [ "2" ]
          ]
      in
      let idoms = get_idoms g in
      print_s [%sexp (idoms : (Label.t, Label.t) Hashtbl.t)];
      [%expect
        {|
        ((((name 1) (id 3)) ((name start) (id 0)))
         (((name 2) (id 4)) ((name start) (id 0)))
         (((name 3) (id 2)) ((name start) (id 0)))
         (((name 4) (id 1)) ((name start) (id 0)))
         (((name start) (id 0)) ((name start) (id 0)))) |}]
    ;;

    (* Figure 4 *)
    let%expect_test _ =
      let g =
        graph
          [ "start", [ "5"; "4" ]
          ; "5", [ "1" ]
          ; "4", [ "2"; "3" ]
          ; "1", [ "2" ]
          ; "2", [ "1"; "3" ]
          ; "3", [ "2" ]
          ]
      in
      let idoms = get_idoms g in
      print_s [%sexp (idoms : (Label.t, Label.t) Hashtbl.t)];
      [%expect
        {|
        ((((name 1) (id 3)) ((name start) (id 0)))
         (((name 2) (id 4)) ((name start) (id 0)))
         (((name 3) (id 2)) ((name start) (id 0)))
         (((name 4) (id 1)) ((name start) (id 0)))
         (((name 5) (id 5)) ((name start) (id 0)))
         (((name start) (id 0)) ((name start) (id 0)))) |}]
    ;;
  end)
;;
