open O
open Types

type t = (Value.t, int * Value.t Instr.t) Entity.Map.t

open Types

let create iter =
  let instr_of_value = ValueMap.create () in
  F.Iter.enumerate iter ~f:(fun (i, Some_instr.T instr) ->
    match instr with
    | Generic_instr.Instr instr ->
      Instr.iter_defs instr ~f:(fun use ->
        ValueMap.set instr_of_value ~key:use ~data:(i, instr))
    | _ -> ());
  instr_of_value
;;

let find_data = ValueMap.find
let find m v = find_data m v |> Option.map ~f:snd
let find_index m v = find_data m v |> Option.map ~f:fst
