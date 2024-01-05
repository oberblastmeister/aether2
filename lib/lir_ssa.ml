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

module Rename : sig
  val rename_function : Function.t -> Function.t
end = struct
  module StringHashtbl = Hashtbl.Make (String)

  type t = { generation_of_name : int StringHashtbl.t }

  let rename_def (st : t) (def : Value.t) =
    let s = Name.pretty def.name in
    Hashtbl.change st.generation_of_name s ~f:(function
      | None -> Some 0
      | Some i -> Some (i + 1));
    let gen = Hashtbl.find_exn st.generation_of_name s in
    { def with name = Name.GenName (s, gen) }

  let rename_use (st : t) (use : Value.t) =
    let s = Name.pretty use.name in
    (* the use should always be in the map because we renamed defs before uses *)
    (* we also made sure that each block had all the live variables as block args which are defs *)
    (* this means that this should never panic *)
    let gen = Hashtbl.find_exn st.generation_of_name s in
    { use with name = Name.GenName (s, gen) }

  let rename_block (st : t) (block : Block.t) =
    Block.map_instrs_forwards
      {
        f =
          (fun i ->
            let i = Instr.map_uses i ~f:(rename_use st) in
            let i = Instr.map_defs i ~f:(rename_def st) in
            i);
      }
      block

  let rename_graph (st : t) (graph : Graph.t) =
    (* very important! we need to rename the start block first because we just renamed the parameters *)
    Graph.map_simple_order graph ~f:(fun (_label, block) ->
        rename_block st block)

  let new_state () = { generation_of_name = StringHashtbl.create () }

  let rename_function (fn : Function.t) =
    let st = new_state () in
    let params = List.map ~f:(rename_def st) fn.params in
    let body = rename_graph st fn.body in
    { fn with params; body }
end

let convert_naive_ssa (fn : Function.t) : Function.t =
  let liveness = Liveness.run fn.body in
  let add_block_args_and_calls label (block : Block.t) =
    let new_entry_instr =
      Instr.Block_args
        (if [%equal: Label.t] label fn.body.entry then []
         else Map.find_exn liveness label |> Set.to_list)
    in
    let new_exit_instr =
      block.exit
      |> (Instr.map_control & InstrControl.map_block_calls)
           ~f:(fun block_call ->
             {
               block_call with
               args = Map.find_exn liveness block_call.label |> Set.to_list;
             })
    in
    { block with entry = new_entry_instr; exit = new_exit_instr }
  in
  let function_with_block_args =
    (Field.map Function.Fields.body & Graph.map_simple_order)
      ~f:(fun (label, block) -> add_block_args_and_calls label block)
      fn
  in
  Rename.rename_function function_with_block_args
