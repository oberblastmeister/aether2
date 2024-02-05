open O
open Utils.Instr_types
open Types
module NameMap = Entity.Map.Make (Name)

module Make (Config : Config) = struct
  module Allocation = struct
    type t = unit [@@deriving sexp]

    let find_exn t name = todo ()
    let did_use_reg _ = todo ()
    let to_iter _ = todo ()
  end

  let run ~precolored:_ ~register_order:_ ~interference =
    let alloc_of_name =
      Interference.nodes interference
      |> F.Iter.map ~f:(fun node -> node, Alloc_reg.Spilled)
      |> NameMap.of_iter ~size:(Interference.size interference)
    in
    Ok ()
  ;;
end
