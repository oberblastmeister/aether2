open O

module type NodeType = sig
  type t [@@deriving equal, hash, sexp, compare]
end

module type GraphTypes = sig
  module Node : NodeType

  type t [@@deriving sexp_of]
end

module type Graph = sig
  include GraphTypes

  val successors_fold : Node.t -> (Node.t, t) G.Fold.t
  val successors : t -> Node.t -> Node.t list
  val predecessors_staged : t -> (Node.t -> Node.t list) Staged.t
end

module type SingleEntryGraph = sig
  include Graph

  val entry : t -> Node.t
end

module Dfs (Graph : Graph) = struct
  let visit visitor start_nodes graph =
    let visited = Hash_set.create (module Graph.Node) in
    let rec go node =
      Hash_set.add visited node;
      visitor (`Enter node);
      G.Fold.iter (Graph.successors_fold node) graph ~f:(fun node ->
        if Hash_set.mem visited node then visitor (`Cycle node) else go node);
      visitor (`Exit node)
    in
    List.iter ~f:go start_nodes
  ;;

  let visit_preorder visitor =
    visit (function
      | `Enter node -> visitor node
      | _ -> ())
  ;;

  let visit_postorder visitor =
    visit (function
      | `Exit node -> visitor node
      | _ -> ())
  ;;

  let postorder start_nodes graph =
    let vec = Vec.create () in
    visit_postorder (Vec.push vec) start_nodes graph;
    vec
  ;;

  let reverse_postorder start_nodes graph =
    let vec = postorder start_nodes graph in
    Vec.rev_inplace vec;
    vec
  ;;

  let preorder start_nodes graph =
    let vec = Vec.create () in
    visit_preorder (Vec.push vec) start_nodes graph;
    vec
  ;;
end

let predecessors_of_map_graph_generic
  ~empty
  ~singleton
  ~combine
  ~fold_outer
  ~fold_inner
  graph
  =
  G.Fold.reduce
    G.Fold.(
      fold_outer @> ix fold_inner @> of_fn (fun (x, y) -> y, x) @> ix (of_fn singleton))
    (G.Reduce.to_map_combine empty ~combine)
    graph
;;

let predecessors_of_map_graph ~empty ~singleton graph =
  G.Fold.reduce
    G.Fold.(
      G.Core.Map.ifold
      @> ix G.Core.Set.fold
      @> of_fn (fun (x, y) -> y, x)
      @> ix (of_fn singleton))
    (G.Reduce.to_map_combine empty ~combine:Set.union)
    graph
;;

module Make_map_graph_using_comparator (Node : sig
    include NodeType
    include Comparable.S with type t := t
  end) : Graph with module Node = Node and type t = Node.Set.t Node.Map.t = struct
  type t = Node.Set.t Node.Map.t [@@deriving sexp_of]

  module Node = Node

  let successors graph node = Map.find_exn graph node |> Set.to_list

  let successors_fold node =
    G.Fold.premap (fun graph -> Map.find_exn graph node) @@ G.Core.Set.fold
  ;;

  let predecessors_staged graph =
    let preds =
      predecessors_of_map_graph ~empty:Node.Map.empty ~singleton:Node.Set.singleton graph
    in
    Staged.stage (fun node ->
      Map.find preds node |> Option.value ~default:Node.Set.empty |> Set.to_list)
  ;;
end

module Make_map_graph (Node : NodeType) = Make_map_graph_using_comparator (struct
    include Node
    include Comparable.Make (Node)
  end)

let%test_module _ =
  (module struct
    module TestGraph = Make_map_graph_using_comparator (String)
    module Dfs = Dfs (TestGraph)

    let%expect_test _ =
      let g =
        String.Map.of_alist_exn
          [ "a", String.Set.of_list [ "b"; "c" ]
          ; "b", String.Set.of_list [ "d" ]
          ; "c", String.Set.of_list [ "d" ]
          ; "d", String.Set.of_list [ "d" ]
          ]
      in
      let res = Dfs.preorder [ "a" ] g |> Vec.to_list in
      print_s [%sexp (res : string list)];
      let res = Dfs.postorder [ "a" ] g |> Vec.to_list in
      print_s [%sexp (res : string list)];
      let res = Dfs.reverse_postorder [ "a" ] g |> Vec.to_list in
      print_s [%sexp (res : string list)];
      ();
      [%expect {|
        (a b d c)
        (d b c a)
        (a c b d) |}]
    ;;
  end)
;;
