(* TODO: don't make idom of start start, that is cylic *)
(*
   https://www.cs.rice.edu/~keith/EMBED/dom.pdf
*)

open! O
open Utils.Instr_types

module Idoms = struct
  type t = Label.t Label.Table.t [@@deriving sexp_of]

  let find = Label.Table.find
  let dominates _ = todo [%here]
end

module Domtree = struct
  type t = Label.t list Label.Table.t [@@deriving sexp_of]

  let children idoms label = Label.Table.find idoms label |> Option.value ~default:[]

  let of_idoms idoms =
    Entity.Map.iteri idoms
    |> F.Iter.map ~f:(fun (label, idom) -> idom, label)
    |> Label.Table.of_iter_accum ~init:[] ~f:(fun acc label -> label :: acc)
  ;;
end

module Frontier = struct
  type t = Label.Set.t Label.Table.t [@@deriving sexp_of]

  let find (t : t) label = (Option.iter @> Set.iter) (Label.Table.find t label)
end

let get_idoms ?node_length ~start (graph : Label.t Data.Graph.double) =
  let idoms = Label.Table.create ?size:node_length () in
  let with_processed label f =
    Label.Table.set idoms ~key:label ~data:label;
    let res = f () in
    Label.Table.remove idoms label;
    res
  in
  let is_processed label = Label.Table.mem idoms label in
  let@ () = with_processed start in
  (* This is needed so that the start is known to be already processed *)
  (* We will remove this after everything is done *)
  Label.Table.set idoms ~key:start ~data:start;
  (* get the nodes *)
  let nodes =
    Data.Graph.Dfs.postorder
      ~start:[ start ]
      ~set:(Data.Constructors.some_hashset (module Label))
      (Data.Graph.t_of_double graph)
  in
  let index_of_node = Label.Table.create ~size:(Vec.length nodes) () in
  (* since we iterate in postorder, things higher up the REVERSE POST ORDER tree will have a higher index *)
  Vec.iteri nodes ~f:(fun i node -> Label.Table.set index_of_node ~key:node ~data:i);
  let intersect node1 node2 =
    let with_index node = node, Label.Table.find_exn index_of_node node in
    let go_up node = with_index (Label.Table.find_exn idoms node) in
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
    (* fold from the right for REVERSE POST ORDER *)
    Vec.fold_right nodes ~init:false ~f:(fun node changed ->
      if [%equal: Label.t] start node
      then changed
      else (
        let preds = F.Fold.to_list graph.preds node in
        (* set the fake idom to the first processed idom *)
        let idom_approx =
          preds
          |> List.find ~f:is_processed
          |> Option.value_exn
               ~message:
                 "must have idom because reverse post order guarantees that we have \
                  processed at least one predecessor"
        in
        let other_preds =
          preds |> List.filter ~f:(fun pred -> not @@ [%equal: Label.t] pred idom_approx)
        in
        let new_idom =
          List.iter other_preds
          |> F.Iter.filter ~f:is_processed
          |> F.Iter.fold ~init:idom_approx ~f:(fun current_idom pred ->
            intersect pred current_idom)
        in
        let changed =
          not @@ [%equal: Label.t option] (Label.Table.find idoms node) (Some new_idom)
        in
        Label.Table.set idoms ~key:node ~data:new_idom;
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
  let frontier_of_node = Label.Table.create () in
  let rec add_until node node_idom runner =
    if not @@ Label.equal runner node_idom
    then (
      (* add node to runner's frontier set because runner doesn't dominate node *)
      Label.Table.update frontier_of_node runner ~f:(function
        | None -> Set.singleton (module Label) node
        | Some fs -> Set.add fs node);
      add_until node node_idom (Label.Table.find_exn idoms runner);
      ())
  in
  graph.all_nodes
  |> F.Iter.iter ~f:(fun node ->
    let num_preds = graph.preds node |> F.Iter.length in
    if is_join_point num_preds
    then
      F.Iter.iter (graph.preds node) ~f:(fun pred ->
        (* every node must have an idom *)
        add_until node (Label.Table.find_exn idoms node) pred));
  frontier_of_node
;;

let%test_module _ =
  (module struct
    module Intern_table = Entity.Intern_table

    let tbl = Intern_table.create (module Label)
    let lab = Intern_table.name_of_string tbl

    let graph xs =
      xs
      |> List.map ~f:(fun (n, ns) -> lab n, List.map ~f:lab ns)
      |> Label.Map.of_alist_exn
      |> Data.Graph.t_of_map_list
      |> Data.Graph.double_of_t (Data.Constructors.some_hashtbl (module Label))
    ;;

    let get_idoms = get_idoms ~start:(lab "start")

    type test =
      { idoms : Idoms.t
      ; frontier : Frontier.t
      ; domtree : Domtree.t
      }
    [@@deriving sexp_of]

    let run_test g =
      let idoms = get_idoms g in
      let frontier = frontier_of_idoms idoms g in
      let domtree = Domtree.of_idoms idoms in
      let test = { idoms; frontier; domtree } in
      print_s (sexp_of_test test)
    ;;

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
      run_test g;
      [%expect
        {|
        ((idoms ((4.1 start.0) (3.2 start.0) (1.3 start.0) (2.4 start.0)))
         (frontier ((4.1 (1.3)) (3.2 (2.4)) (1.3 (2.4)) (2.4 (1.3))))
         (domtree ((start.0 (2.4 1.3 3.2 4.1))))) |}]
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
      run_test g;
      [%expect
        {|
        ((idoms
          ((4.1 start.0) (3.2 start.0) (1.3 start.0) (2.4 start.0) (5.5 start.0)))
         (frontier
          ((4.1 (2.4 3.2)) (3.2 (2.4)) (1.3 (2.4)) (2.4 (1.3 3.2)) (5.5 (1.3))))
         (domtree ((start.0 (5.5 2.4 1.3 3.2 4.1))))) |}]
    ;;
  end)
;;
