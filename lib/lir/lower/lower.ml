open! O
module Lir = Ast
module Tir = Tir

let iter_blocks graph = (Cfg.Graph.iter_on_labels (Lir.Graph.Dfs.preorder graph)) graph

let iter_instrs fn =
  (iter_blocks @> F.Fold.mapped snd @> Lir.Block.iter_instrs_forward) fn
;;

module Context = struct
  type t =
    { (* remove if not used *)
      use_states : Use_states.t
    ; color_of_index : Side_effects.Color_of_index.t
    ; instr_of_value : Instr_of_value.t
    ; instrs : (Vir.Some_instr.t, read) Vec.t
    ; inlined_indices : Int.Hash_set.t
    }

  let get_color cx instr_index =
    Side_effects.Color_of_index.get cx.color_of_index instr_index
  ;;

  let create (fn : Vir.Function.t) =
    let instrs = iter_instrs fn.graph |> Vec.of_iter |> Vec.freeze in
    let instr_of_value = Instr_of_value.create (Vec.iter instrs) in
    let color_of_index = Side_effects.color fn.graph in
    let use_states = Use_states.create fn instr_of_value in
    let inlined_indices = Int.Hash_set.create () in
    { instrs; instr_of_value; use_states; color_of_index; inlined_indices }
  ;;

  let add_inlined_index cx index = Hash_set.add cx.inlined_indices index
  let is_index_inlined cx index = Hash_set.mem cx.inlined_indices index
end

let rec lower_instr (cx : Context.t) (instr : Vir.Instr.t) color =
  (* let color = Context.get_color cx instr_index in *)
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
  (* can't inline, this isn't a single value Instr.t *)
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
      (* IMPORTANT: lower the value instr with the current instrs color *)
      match lower_instr cx value_instr color with
      | Lir.Instr.Assign { dst; expr } ->
        assert (Lir.Value.equal dst value);
        Context.add_inlined_index cx index;
        Tir.Value.I { dst; expr }
      | Lir.Instr.ImpureAssign { dst; expr } ->
        assert (Lir.Value.equal dst value);
        Context.add_inlined_index cx index;
        Tir.Value.I' { dst; expr }
      | instr ->
        raise_s
          [%message
            "expected an assign instr if can_inline" (instr : Tir.Value.t Lir.Instr.t)])
    else Tir.Value.V value
  | Some _ -> Tir.Value.V value
;;

let lower_block cx (block : Vir.Block.t) instr_index =
  (* incr instr_index; *)
  let length = List.length block.body in
  let index_start = !instr_index in
  let body_index_start = index_start + 1 in
  let exit = lower_control_instr cx block.exit (index_start + 1 + length) in
  let body =
    List.rev_filter_mapi (List.rev block.body) ~f:(fun i instr ->
      let index = body_index_start + (length - i - 1) in
      let res =
        if Context.is_index_inlined cx index
        then (* this instruction was inlined, so skip it *)
          None
        else Some (lower_instr cx instr (Context.get_color cx index))
      in
      res)
  in
  let entry = lower_block_call cx block.entry index_start in
  instr_index := 2 + length + !instr_index;
  { Lir.Block.entry; body; exit }
;;

let lower_fn (fn : Vir.Function.t) =
  let cx = Context.create fn in
  let instr_index = ref 0 in
  let blocks =
    iter_blocks fn.graph
    |> F.Iter.map ~f:(fun (label, block) -> label, lower_block cx block instr_index)
    |> F.Iter.to_list
  in
  Lir.Function.map_graph fn ~f:(fun graph -> Cfg.Graph.set_blocks_alist blocks graph)
;;

let run program = (Lir.Program.map_functions & List.map) program ~f:lower_fn
