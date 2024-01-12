open O
open Data_graph_types
open Data_graph_basic

let visit visitor start_nodes (graph : _ t) =
  let visited = Hash_set.create (node_to_key graph.node) in
  let rec go node =
    Hash_set.add visited node;
    visitor (`Enter node);
    F.Fold.iter graph.succs node ~f:(fun node ->
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
