open! O
module Lir = Types

let blocks_fold graph = (Cfg.Graph.fold_labels (Lir.Graph.Dfs.preorder graph)) graph
let instr_fold fn = (blocks_fold @> F.Fold.mapped snd @> Lir.Block.instrs_forward_fold) fn

module Context = struct
  type t =
    { (* remove if not used *)
      use_states : Use_states.t
    ; color_of_index : Side_effects.Color_of_index.t
    ; instr_of_value : Instr_of_value.t
    ; instrs : (Vir.Some_instr.t, read) Vec.t
    }

  let get_color cx instr_index =
    Side_effects.Color_of_index.get cx.color_of_index instr_index
  ;;

  let create (fn : Vir.Function.t) =
    let instrs = instr_fold fn.graph |> Vec.of_iter |> Vec.freeze in
    let instr_of_value = Instr_of_value.create (Vec.to_iter instrs) in
    let color_of_index = Side_effects.color fn.graph in
    let use_states = Use_states.create fn instr_of_value in
    { instrs; instr_of_value; use_states; color_of_index }
  ;;
end

let rec lower_instr (cx : Context.t) (instr : Vir.Instr.t) instr_index =
  let color = Context.get_color cx instr_index in
  Lir.Instr.map_uses instr ~f:(fun use -> lower_value cx color use)

and lower_block_call (_cx : Context.t) (block_args : Vir.Block_args.t) _instr_index
  : Tir.Block_args.t
  =
  block_args

and lower_control_instr (cx : Context.t) (instr : Vir.Control_instr.t) instr_index =
  let color = Context.get_color cx instr_index in
  Lir.Control_instr.map_uses instr ~f:(fun use -> lower_value cx color use)

and lower_value cx color value =
  let data = Instr_of_value.find_data cx.instr_of_value value in
  match data with
  (* can't inline, this isn't an actual Instr.t *)
  | None -> Tir.Value.V value
  (* used once, possibly inline *)
  | Some (index, value_instr)
    when [%equal: Use_states.state] (Use_states.find cx.use_states value) Once ->
    let value_instr_color = Side_effects.Color_of_index.get cx.color_of_index index in
    let can_inline =
      (not (Lir.Instr.has_side_effect value_instr))
      || Side_effects.Color.is_adjacent value_instr_color color
    in
    if can_inline
    then Tir.Value.I (lower_instr cx value_instr index)
    else Tir.Value.V value
  | Some _ -> Tir.Value.V value
;;

let lower_block cx (block : Vir.Block.t) instr_index =
  let entry = lower_block_call cx block.entry !instr_index in
  incr instr_index;
  let body =
    List.filter_map block.body ~f:(fun instr ->
      let res =
        if F.Iter.exists (Lir.Instr.defs_fold instr) ~f:(fun def ->
             [%equal: Use_states.state] (Use_states.find cx.use_states def) Once)
        then
          (* this instruction will be inlined later, so don't duplicate it by lowering it again *)
          None
        else Some (lower_instr cx instr !instr_index)
      in
      incr instr_index;
      res)
  in
  let exit = lower_control_instr cx block.exit !instr_index in
  incr instr_index;
  { Lir.Block.entry; body; exit }
;;

let lower_fn (fn : Vir.Function.t) =
  let cx = Context.create fn in
  let instr_index = ref 0 in
  let blocks =
    blocks_fold fn.graph
    |> F.Iter.map ~f:(fun (label, block) -> label, lower_block cx block instr_index)
    |> F.Iter.fold
         ~f:(fun m (label, block) -> Map.set m ~key:label ~data:block)
         ~init:Lir.Label.Map.empty
  in
  let fn = Lir.Function.map_blocks fn ~f:(Fn.const blocks) in
  fn
;;

let lower program = (Lir.Program.map_functions & List.map) program ~f:lower_fn
