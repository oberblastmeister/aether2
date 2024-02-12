open O
open Utils.Instr_types
open Types
module NameMap = Entity.Map.Make (Name)

module Make (Config : Config) = struct
  open Config

  let run ~precolored:_ ~interference =
    let alloc_of_name =
      Interference.nodes interference
      |> F.Iter.map ~f:(fun node -> node, Alloc_reg.Spilled)
      |> NameMap.of_iter ~size:(Interference.size interference)
    in
    Ok (todo ())
  ;;
end
