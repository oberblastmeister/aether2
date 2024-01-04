open O
open Lir_instr

module PhiValue = struct
  type 'v t = { flowed_from_label : Label.t; dest : 'v; value : 'v }
  [@@deriving sexp, fields]
end

module Phi = struct
  type 'v t = {
    dest_label : Label.t;
    dest : 'v;
    flow_values : 'v PhiValue.t list;
  }
  [@@deriving sexp, fields]
end

let convert_naive_ssa (fn : Function.t) : Function.t =
  let liveness = Liveness.run fn.body in
  let add_block_args_and_calls label block =
    let new_entry_instr =
      Instr.Block_args
        (if [%equal: Label.t] label fn.body.entry then []
         else Map.find_exn liveness label |> Set.to_list)
    in
    let new_exit_instr = todo () in
    todo ()
  in
  todo ()
