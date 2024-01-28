open! O
open Utils.Instr_types
module Table = Entity.Map.Make (Label)

module Idoms = struct
  type t = (Label.t, Label.t) Entity.Map.t [@@deriving sexp_of]

  let find = Table.find
end

module Frontier = struct
  type t = (Label.t, Label.t Hash_set.t) Entity.Map.t [@@deriving sexp_of]

  let find (t : t) label = (FC.Option.fold @> FC.Hash_set.fold) (Table.find t label)
end

let get_idoms ?node_length ~start (graph : Label.t Data.Graph.double) =
  let module Table = Entity.Map.Make (Label) in
  let idoms = Table.create ?size:node_length () in
  (* special case for start node *)
  Table.set idoms ~key:start ~data:start;
  let nodes =
    Data.Graph.Dfs.postorder
      ~start:[ start ]
      ~set:(Data.Constructors.some_hashset (module Label))
      (Data.Graph.t_of_double graph)
  in
  let index_of_node = Hashtbl.create ~size:(Vec.length nodes) (module Label) in
  Vec.iteri nodes ~f:(fun i node -> Hashtbl.set index_of_node ~key:node ~data:i);
  let intersect node1 node2 =
    (* print_s
       [%message "intersect" ~node1:(node1 : Graph.Node.t) ~node2:(node2 : Graph.Node.t)]; *)
    let with_index node = node, Hashtbl.find_exn index_of_node node in
    let go_up node = with_index (Table.find_exn idoms node) in
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
      if [%equal: Label.t] start node
      then changed
      else (
        let preds = F.Fold.to_list graph.preds node in
        (* print_s [%message "compute" ~node:(node : Node.t) ~preds:(preds : Node.t list)]; *)
        let chosen_pred =
          preds
          |> List.find ~f:(Table.mem idoms)
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
            match Table.find idoms pred with
            | None -> current_idom
            | Some pred_idom -> intersect pred_idom current_idom)
        in
        (* print_s [%message "new_idom" (node : Node.t) (new_idom : Node.t)]; *)
        let changed =
          not @@ [%equal: Label.t option] (Table.find idoms node) (Some new_idom)
        in
        Table.set idoms ~key:node ~data:new_idom;
        changed))
  in
  let rec fixpoint () =
    match compute () with
    | true -> fixpoint ()
    | false -> ()
  in
  fixpoint ();
  idoms
;;

let frontier_of_idoms idoms (graph : Label.t Data.Graph.double) =
  let is_join_point preds_length = preds_length >= 2 in
  let frontier_of_node = Table.create () in
  let rec add_until node node_idom runner =
    if not @@ [%equal: Label.t] runner node_idom
    then (
      (* add node to runner's frontier set because runner doesn't dominate node *)
      match Table.find frontier_of_node runner with
      | None ->
        Table.set
          frontier_of_node
          ~key:runner
          ~data:
            (let set = Hash_set.create (module Label) in
             Hash_set.add set node;
             set)
      | Some fs ->
        Hash_set.add fs node;
        add_until node node_idom (Table.find_exn idoms runner))
  in
  graph.all_nodes
  |> F.Iter.iter ~f:(fun node ->
    let preds = graph.preds node |> Iter.length in
    if is_join_point preds
    then
      F.Iter.iter (graph.preds node) ~f:(fun pred ->
        (* every node must have an idom *)
        add_until node (Table.find_exn idoms node) pred));
  frontier_of_node
;;

let%test_module _ =
  (module struct
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
      |> Data.Graph.t_of_map_list
      |> Data.Graph.double_of_t (Data.Constructors.some_hashtbl (module Label))
    ;;

    let get_idoms = get_idoms ~start:(lab "start")

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
      print_s [%sexp (idoms : Idoms.t)];
      [%expect
        {|
        ((((name start) (id 0)) ((name start) (id 0)))
         (((name 4) (id 1)) ((name start) (id 0)))
         (((name 3) (id 2)) ((name start) (id 0)))
         (((name 1) (id 3)) ((name start) (id 0)))
         (((name 2) (id 4)) ((name start) (id 0)))) |}]
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
      print_s [%sexp (idoms : Idoms.t)];
      [%expect
        {|
        ((((name start) (id 0)) ((name start) (id 0)))
         (((name 4) (id 1)) ((name start) (id 0)))
         (((name 3) (id 2)) ((name start) (id 0)))
         (((name 1) (id 3)) ((name start) (id 0)))
         (((name 2) (id 4)) ((name start) (id 0)))
         (((name 5) (id 5)) ((name start) (id 0)))) |}]
    ;;
  end)
;;
