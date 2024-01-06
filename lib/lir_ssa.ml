open O
open Lir_instr

module ValueWithId = struct
  type t =
    { value : Value.t
    ; id : Value.t Union_find.t [@equal.ignore]
    }
  [@@deriving equal]
end

module PhiValue = struct
  type 'v t =
    { flowed_from_label : Label.t [@equal.ignore]
    ; dest : 'v [@equal.ignore]
    ; value : 'v
    }
  [@@deriving equal, sexp, fields, map]
end

module Phi = struct
  type 'v t =
    { dest_label : Label.t
    ; dest : 'v
    ; flow_values : 'v PhiValue.t list
    }
  [@@deriving sexp, fields, map]

  let map x ~f = map f x
end

let or_error_of_list = function
  | [] -> Ok ()
  | _ :: _ as es -> Error.of_list es |> Error
;;

let check_all_temps_unique (fn : Function.t) =
  let errors : Error.t Stack.t = Stack.create () in
  let defines : Value.Hash_set.t = Value.Hash_set.create () in
  let check_define label instr def =
    if Hash_set.mem defines def
    then
      Stack.push
        errors
        (Error.t_of_sexp
           [%message
             "a temporary was defined more than once"
               ~label:(label : Label.t option)
               ~instr:(instr : Instr.Some.t option)
               ~def:(def : Value.t)]);
    Hash_set.add defines def
  in
  List.iter fn.params ~f:(check_define None None);
  let fold =
    G.Fold.(G.Core.Map.ifold @> ix (dup Block.instrs_forward_fold) @> ix2 Instr.defs_fold)
  in
  G.Fold.iter
    fold
    ~f:(fun (label, (i, def)) -> check_define (Some label) (Some i) def)
    fn.body.blocks;
  Stack.to_list errors |> or_error_of_list
;;

let validate_ssa_function (fn : Function.t) =
  let open Or_error.Let_syntax in
  Graph.validate fn.body;
  let%bind () = check_all_temps_unique fn in
  let dom_tree =
    Dominators.run fn.body |> Cfg.Dominators.compute_idom_tree_from_facts fn.body.entry
  in
  let errors : Error.t Stack.t = Stack.create () in
  let defined_in_dominators =
    List.fold fn.params ~init:Value.Set.empty ~f:(fun z param -> Set.add z param)
  in
  (* traverse the dominator tree *)
  (* it will terminated because it is a tree not a graph *)
  let rec go label defined_in_dominators =
    let block = Map.find_exn fn.body.blocks label in
    let defined_in_dominators =
      Block.fold_instrs_forward
        block
        ~init:defined_in_dominators
        ~f:(fun defined_in_dominators (Instr.Some.T instr as i) ->
          let uses = Instr.uses instr in
          List.iter uses ~f:(fun use ->
            if Set.mem defined_in_dominators use |> not
            then
              Stack.push
                errors
                (Error.t_of_sexp
                   [%message
                     "a use was not dominated by a define"
                       ~instr:(i : Instr.Some.t)
                       ~use:(use : Value.t)
                       ~label:(label : Label.t)
                       ~defines:(defined_in_dominators : Value.Set.t)]));
          let defs = Instr.defs instr in
          let defined_in_dominators =
            List.fold defs ~init:defined_in_dominators ~f:(fun z def -> Set.add z def)
          in
          defined_in_dominators)
    in
    let children =
      Map.find dom_tree label |> Option.value ~default:Label.Set.empty |> Set.to_list
    in
    List.iter children ~f:(fun child -> go child defined_in_dominators)
  in
  go fn.body.entry defined_in_dominators;
  Stack.to_list errors
  |> or_error_of_list
  |> Result.map_error ~f:(Error.tag_s ~tag:[%message "in function" (fn.name : string)])
;;

let validate_ssa (prog : Program.t) =
  List.iter prog.functions ~f:(fun fn -> validate_ssa_function fn |> Or_error.ok_exn)
;;

module Rename : sig
  val rename_function : Function.t -> Function.t
end = struct
  module StringHashtbl = Hashtbl.Make (String)

  type t = { generation_of_name : int StringHashtbl.t }

  let rename_def (st : t) (def : Value.t) =
    let s = Name.pretty def.name in
    Hashtbl.update st.generation_of_name s ~f:(function
      | None -> 0
      | Some i -> i + 1);
    let gen = Hashtbl.find_exn st.generation_of_name s in
    { def with name = Name.GenName (s, gen) }
  ;;

  let rename_use (st : t) (use : Value.t) =
    let s = Name.pretty use.name in
    (* the use should always be in the map because we renamed defs before uses *)
    (* we also made sure that each block had all the live variables as block args which are defs *)
    (* this means that this should never panic *)
    let gen = Hashtbl.find_exn st.generation_of_name s in
    { use with name = Name.GenName (s, gen) }
  ;;

  let rename_block (st : t) (block : Block.t) =
    Block.map_instrs_forwards
      { f =
          (fun i ->
            let i = Instr.map_uses i ~f:(rename_use st) in
            let i = Instr.map_defs i ~f:(rename_def st) in
            i)
      }
      block
  ;;

  let rename_graph (st : t) (graph : Graph.t) =
    (* very important! we need to rename the start block first because we just renamed the parameters *)
    Graph.map_simple_order graph ~f:(fun (_label, block) -> rename_block st block)
  ;;

  let new_state () = { generation_of_name = StringHashtbl.create () }

  let rename_function (fn : Function.t) =
    let st = new_state () in
    let params = List.map ~f:(rename_def st) fn.params in
    let body = rename_graph st fn.body in
    { fn with params; body }
  ;;
end

let convert_naive_ssa (fn : Function.t) : Function.t =
  let liveness = Liveness.run fn.body in
  let add_block_args_and_calls label (block : Block.t) =
    let new_entry_instr =
      Instr.Block_args
        (if [%equal: Label.t] label fn.body.entry
         then []
         else Map.find_exn liveness label |> Set.to_list)
    in
    let new_exit_instr =
      block.exit
      |> (Instr.map_control & InstrControl.map_block_calls) ~f:(fun block_call ->
        { block_call with args = Map.find_exn liveness block_call.label |> Set.to_list })
    in
    { block with entry = new_entry_instr; exit = new_exit_instr }
  in
  let function_with_block_args =
    (Field.map Function.Fields.body & Graph.map_simple_order)
      ~f:(fun (label, block) -> add_block_args_and_calls label block)
      fn
  in
  let renamed_function = Rename.rename_function function_with_block_args in
  validate_ssa_function renamed_function |> Or_error.ok_exn;
  renamed_function
;;

let get_phis (graph : Graph.t) =
  let initial_phis =
    G.Core.Map.mapi graph.blocks ~f:(fun (label, block) ->
      block.entry
      |> Instr.get_block_args
      |> List.map ~f:(fun arg : _ Phi.t ->
        { dest_label = label; dest = arg; flow_values = [] }))
  in
  let fold =
    G.Fold.(
      G.Core.Map.ifold
      @> ix (of_fn Block.exit @> of_fn Instr.get_control @> InstrControl.block_calls_fold))
  in
  let phis_of_block =
    G.Fold.fold
      fold
      ~init:initial_phis
      ~f:(fun phi_map (label, (block_call : BlockCall.t)) ->
        let phis = Map.find_exn phi_map block_call.label in
        let phis =
          match List.zip phis block_call.args with
          | Ok res ->
            res
            |> List.map ~f:(fun (phi, value) ->
              let phi_value : _ PhiValue.t =
                { flowed_from_label = label; dest = phi.dest; value }
              in
              { phi with flow_values = phi_value :: phi.flow_values })
          | _ -> raise_s [%message "called with more args then the block has"]
        in
        Map.set phi_map ~key:block_call.label ~data:phis)
      graph.blocks
  in
  phis_of_block
;;

type value_id = Value.t Union_find.t

let simplify_phis (phis : Value.t Phi.t list) =
  let phis_with_id : value_id Phi.t list =
    (List.map & Phi.map) ~f:(fun value -> Union_find.create value) phis
  in
  let zonk_phi_final = Phi.map ~f:Union_find.get in
  let zonk_phi =
    Phi.map ~f:(fun id : ValueWithId.t -> { value = Union_find.get id; id })
  in
  let is_removable (phi : ValueWithId.t Phi.t) =
    let other_values =
      List.filter phi.flow_values ~f:(fun phi_value ->
        not @@ [%equal: ValueWithId.t] phi.dest phi_value.value)
    in
    List.all_equal other_values ~equal:(fun x y -> [%equal: ValueWithId.t PhiValue.t] x y)
  in
  let subst = Value.Hashtbl.create () in
  let rec try_remove (phis : value_id Phi.t list) =
    match phis with
    | [] -> `Didn'tRemove, []
    | phi :: phis ->
      let zonked_phi = zonk_phi phi in
      (match is_removable zonked_phi with
       | None ->
         let removed, phis = try_remove phis in
         removed, phi :: phis
       | Some replacement ->
         Union_find.union phi.dest replacement.value.id;
         Union_find.set phi.dest replacement.value.value;
         Hashtbl.set ~key:zonked_phi.dest.value ~data:replacement.value.id subst;
         (* the phi variable is not consed back on here, we removed it *)
         `Removed, phis)
  in
  let rec fixpoint phis =
    match try_remove phis with
    | `Didn'tRemove, phis -> phis
    | `Removed, phis -> fixpoint phis
  in
  (* probably need to create a substitution at the end eventually *)
  let simplified_phis = fixpoint phis_with_id |> List.map ~f:zonk_phi_final in
  let zonked_subst = Hashtbl.map subst ~f:Union_find.get in
  zonked_subst, simplified_phis
;;
