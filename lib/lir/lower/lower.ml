open! O
module Lir = Types
module Tir = Tir

let blocks_fold graph = (Cfg.Graph.iter_on_labels (Lir.Graph.Dfs.preorder graph)) graph
let instr_fold fn = (blocks_fold @> F.Fold.mapped snd @> Lir.Block.iter_instrs_forward) fn

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
    let instr_of_value = Instr_of_value.create (Vec.iter instrs) in
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
    when [%equal: Use_states.state option]
           (Use_states.find cx.use_states value)
           (Some Once) ->
    let value_instr_color = Side_effects.Color_of_index.get cx.color_of_index index in
    let can_inline =
      (not (Lir.Instr.has_side_effect value_instr))
      || Side_effects.Color.is_before value_instr_color color
    in
    if can_inline
    then (
      match lower_instr cx value_instr index with
      | Lir.Instr.Assign { dst; expr } -> Tir.Value.I { dst; expr }
      | _ -> raise_s [%message "expected an assign instr if can_inline"])
    else Tir.Value.V value
  | Some _ -> Tir.Value.V value
;;

let lower_block cx (block : Vir.Block.t) instr_index =
  let entry = lower_block_call cx block.entry !instr_index in
  incr instr_index;
  let body =
    List.filter_map block.body ~f:(fun instr ->
      let res =
        if F.Iter.exists (Lir.Instr.iter_defs instr) ~f:(fun def ->
             [%equal: Use_states.state option]
               (Use_states.find cx.use_states def)
               (Some Once))
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
    |> F.Iter.to_list
  in
  Lir.Function.map_graph fn ~f:(fun graph -> Cfg.Graph.set_blocks_alist blocks graph)
;;

let run program = (Lir.Program.map_functions & List.map) program ~f:lower_fn
