open O
open Instr_types

module DominatorFact = Cfg_dataflow.DominatorFact

(* the idom for start always points to start *)
let compute_idoms_from_facts (start_label : Label.t) (d : DominatorFact.t Label.Map.t) =
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
    G.Fold.reduce
      G.Fold.(
        G.Core.Map.ifold
        @> ix G.Core.Set.fold
        @> of_fn (fun (x, y) -> y, x)
        @> ix (of_fn Label.Set.singleton))
      (G.Reduce.to_map_combine Label.Map.empty ~combine:Set.union)
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
  let visited = Label.Hash_set.create () in
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
    G.Fold.reduce
      G.Fold.(
        G.Core.Map.ifold
        @> of_fn (fun (label, dominates_label) -> dominates_label, label)
        @> ix
             (of_fn (fun label ->
                (* don't add the start label to the idom tree *)
                if [%equal: Label.t] start_label label
                then Label.Set.empty
                else Label.Set.singleton label)))
      (G.Reduce.to_map_combine Label.Map.empty ~combine:Set.union)
      idoms
  in
  assert_is_tree start_label idom_tree;
  idom_tree
;;
