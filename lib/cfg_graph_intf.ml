open O
open Instr_types

module T = struct
  type 'b t =
    { entry : Label.t
    ; blocks : 'b Label.Map.t
    ; exit : Label.t
    }
  [@@deriving sexp_of, fields]
end

include T

let map_blocks graph ~f = { graph with blocks = f graph.blocks }

let set_block graph label block =
  map_blocks graph ~f:(fun blocks -> Map.set ~key:label ~data:block blocks)
;;

let add_block_exn graph label block =
  map_blocks graph ~f:(Map.add_exn ~key:label ~data:block)
;;

let validate graph =
  let _ = Map.find_exn graph.blocks graph.entry in
  let _ = Map.find_exn graph.blocks graph.exit in
  ()
;;

let to_graph ~jumps graph =
  { Graphs.node = (module Label)
  ; succs = (fun label -> jumps (Map.find_exn graph.blocks label))
  ; all_nodes = (fun k -> Map.iter_keys graph.blocks ~f:k)
  }
;;

let to_double_graph ~jumps graph = to_graph ~jumps graph |> Graphs.double_of_t

let predecessors_of_label ~jumps (graph : 'b t) =
  graph.blocks
  |> Map.to_alist
  |> List.bind ~f:(fun (jumped_from, block) ->
    jumps block |> List.map ~f:(fun jumped_to -> jumped_to, jumped_from))
  |> Label.Map.of_alist_multi
;;

let map_simple_order (graph : 'b t) ~(f : Label.t * 'b -> 'b) =
  let start_block =
    Map.find_exn graph.blocks graph.entry |> fun block -> f (graph.entry, block)
  in
  let blocks =
    F.Core.Map.mapi
      ~f:(fun (label, block) ->
        if [%equal: Label.t] label graph.entry || [%equal: Label.t] label graph.exit
        then block
        else f (label, block))
      graph.blocks
  in
  let end_block =
    Map.find_exn graph.blocks graph.exit |> fun block -> f (graph.exit, block)
  in
  let blocks =
    blocks
    |> Map.set ~key:graph.entry ~data:start_block
    |> Map.set ~key:graph.exit ~data:end_block
  in
  { graph with blocks }
;;

module type Block = sig
  type t [@@deriving sexp_of]

  val jumps_fold : (Label.t, t) F.Fold.t
  val jumps : t -> Label.t list
end

module type Intf = sig
  type 'b t =
    { entry : Label.t
    ; blocks : 'b Label.Map.t
    ; exit : Label.t
    }
  [@@deriving sexp_of, fields]

  val to_graph : jumps:('b -> Label.t F.Iter.t) -> 'b t -> Label.t Graphs.t
  val map_blocks : 'b t -> f:('b Label.Map.t -> 'c Label.Map.t) -> 'c t
  val set_block : 'b t -> Label.t -> 'b -> 'b t
  val add_block_exn : 'b t -> Label.t -> 'b -> 'b t
  val validate : 'b t -> unit

  val predecessors_of_label
    :  jumps:('b -> Label.t list)
    -> 'b t
    -> Label.t list Label.Map.t

  val map_simple_order : 'b t -> f:(Label.t * 'b -> 'b) -> 'b t
end
