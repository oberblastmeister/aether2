open! O
open Data_graph_types
open Data_graph_basic

type 'n event =
  | Cycle of 'n
  | Enter of 'n
  | Exit of 'n

let visit (type k) ~f ~start ~(set : k Constructors.some_set) (graph : _ t) =
  let module Set = (val set) in
  let visited = Set.create () in
  let rec go node =
    Set.add visited node;
    f (Enter node);
    F.Fold.iter graph.succs node ~f:(fun node ->
      if Set.mem visited node then f (Cycle node) else go node);
    f (Exit node)
  in
  List.iter ~f:go start
;;

let visit_preorder ~f =
  visit ~f:(function
    | Enter node -> f node
    | _ -> ())
;;

let visit_postorder ~f =
  visit ~f:(function
    | Exit node -> f node
    | _ -> ())
;;

let postorder ~start ~set graph =
  let vec = Vec.create () in
  visit_postorder ~f:(Vec.push vec) ~start ~set graph;
  vec
;;

let reverse_postorder ~start ~set graph =
  let vec = postorder ~start ~set graph in
  Vec.rev_inplace vec;
  vec
;;

let preorder ~start ~set graph =
  let vec = Vec.create () in
  visit_preorder ~f:(Vec.push vec) ~start ~set graph;
  vec
;;
