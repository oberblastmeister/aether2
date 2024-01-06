open O
open Instr_types

module Graph = struct
  type 'b t =
    { entry : Label.t
    ; blocks : 'b Label.Map.t
    ; exit : Label.t
    }
  [@@deriving sexp_of, fields]

  module Stuff = struct
    let entry = entry
    let blocks = blocks
    let exit = exit

    module Fields = Fields

    let validate graph =
      let _ = Map.find_exn graph.blocks graph.entry in
      let _ = Map.find_exn graph.blocks graph.exit in
      ()
    ;;

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
        G.Core.Map.mapi
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
  end
end
