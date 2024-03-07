open O
open Types

type state =
  | Once
  | Multiple
[@@deriving equal, sexp_of]

let inc_state = function
  | None -> Once
  | Some (Once | Multiple) -> Multiple
;;

type t = (Value.t, state) Entity.Map.t [@@deriving sexp_of]

let create fn instr_of_value =
  let use_states = ValueMap.create () in
  (Function.iter_instrs_forward @> Some_instr.iter_uses) fn ~f:(fun use ->
    ValueMap.set
      use_states
      ~key:use
      ~data:
        (let instr = Instr_of_value.find instr_of_value use in
         let prev = ValueMap.find use_states use in
         match instr with
         | Some instr
           when let defs_count = Instr.iter_defs instr |> F.Iter.length in
                defs_count > 1 -> Multiple
         | Some _ | None -> inc_state prev));
  use_states
;;

let find = ValueMap.find_exn
