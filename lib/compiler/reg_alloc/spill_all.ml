open O
open Utils.Instr_types
open Types

module Make (Config : Config) = struct
  let run ~precolored:_ ~interference =
    let alloc_of_name =
      Interference.nodes interference
      |> F.Iter.map ~f:(fun node -> node, Alloc_reg.Spilled)
      |> Name.Table.of_iter ~size:(Interference.size interference)
    in
    Ok (todo ())
  ;;
end
