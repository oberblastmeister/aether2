open! O
open Lir_instr

module ValueWithId = struct
  type t =
    { value : Value.t
    ; id : Value.t Union_find.t [@equal.ignore]
    }
  [@@deriving equal]

  let sexp_of_t v = [%sexp (v.value : Value.t)]
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

let check_all_temps_unique (fn : Value.t Function.t) =
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
               ~instr:(instr : Value.t Some_instr.t option)
               ~def:(def : Value.t)]);
    Hash_set.add defines def
  in
  List.iter fn.params ~f:(check_define None None);
  let fold =
    F.Fold.(
      F.Core.Map.foldi @> ix (dup Block.instrs_forward_fold) @> ix2 Some_instr.defs_fold)
  in
  F.Fold.iter
    fold
    ~f:(fun (label, (i, def)) -> check_define (Some label) (Some i) def)
    fn.graph.blocks;
  Stack.to_list errors |> or_error_of_list
;;

let validate_ssa_function (fn : Vir.Function.t) =
  let open! Or_error.Let_syntax in
  Graph.validate fn.graph;
  let%bind () = check_all_temps_unique fn in
  let labels =
    Data_graph.Dfs.preorder
      ~start:[ fn.graph.entry ]
      ~set:(Constructors.some_hashset (module Label))
      (fn.graph |> Graph.to_graph)
  in
  let errors : Error.t Stack.t = Stack.create () in
  let defined = Value.Hash_set.create () in
  List.iter fn.params ~f:(fun param -> Hash_set.add defined param);
  Vec.iter labels ~f:(fun label ->
    let block = Map.find_exn fn.graph.blocks label in
    F.Fold.iter Block.instrs_forward_fold block ~f:(fun (Some_instr.T instr as i) ->
      let uses = Generic_instr.uses instr in
      List.iter uses ~f:(fun use ->
        if not @@ Hash_set.mem defined use
        then
          Stack.push
            errors
            (Error.t_of_sexp
               [%message
                 "a use was not dominated by a define"
                   ~instr:(i : Vir.Some_instr.t)
                   ~use:(use : Value.t)
                   ~label:(label : Label.t)
                   ~defines:(defined : Value.Hash_set.t)]));
      let defs = Generic_instr.defs instr in
      List.iter defs ~f:(fun def -> Hash_set.add defined def)));
  Stack.to_list errors
  |> or_error_of_list
  |> Result.map_error ~f:(Error.tag_s ~tag:[%message "in function" (fn.name : string)])
;;

let validate_ssa (prog : Vir.Program.t) =
  List.iter prog.functions ~f:(fun fn -> validate_ssa_function fn |> Or_error.ok_exn)
;;

module Rename : sig
  val rename_function : Vir.Function.t -> Vir.Function.t
end = struct
  type t =
    { mutable unique_name : Name.Id.t
    ; generation_of_name : (Name.Id.t, Name.Id.t) Hashtbl.t
    }

  let fresh_name (st : t) =
    let unique = st.unique_name in
    st.unique_name <- Name.Id.next unique;
    unique
  ;;

  let rename_def (st : t) (def : Value.t) =
    let unique = fresh_name st in
    Hashtbl.set st.generation_of_name ~key:def.name.id ~data:unique;
    { def with name = Name.create def.name.name unique }
  ;;

  let rename_use (st : t) (use : Value.t) =
    (* the use should always be in the map because we renamed defs before uses *)
    (* we also made sure that each block had all the live variables as block args which are defs *)
    (* this means that this should never panic *)
    let unique = Hashtbl.find_exn st.generation_of_name use.name.id in
    { use with name = Name.create use.name.name unique }
  ;;

  let rename_block (st : t) (block : Vir.Block.t) =
    Block.map_instrs_forwards
      { f =
          (fun i ->
            let i = Generic_instr.map_uses i ~f:(rename_use st) in
            let i = Generic_instr.map_defs i ~f:(rename_def st) in
            i)
      }
      block
  ;;

  let rename_graph (st : t) (graph : Vir.Graph.t) =
    (* very important! we need to rename the start block first because we just renamed the parameters *)
    Cfg_graph.map_simple_order graph ~f:(fun (_label, block) -> rename_block st block)
  ;;

  let new_state unique_name =
    { generation_of_name = Hashtbl.create (module Name.Id); unique_name }
  ;;

  let rename_function (fn : Vir.Function.t) =
    let st = new_state (Name.Id.of_int 0) in
    let params = List.map ~f:(rename_def st) fn.params in
    let graph = rename_graph st fn.graph in
    { fn with params; graph; unique_name = st.unique_name }
  ;;
end

let convert_naive_ssa (fn : Vir.Function.t) : Vir.Function.t =
  let liveness = Vir.Liveness.run fn.graph in
  let add_block_args_and_calls label (block : Vir.Block.t) =
    let new_entry_instr =
      if [%equal: Label.t] label fn.graph.entry
      then []
      else Map.find_exn liveness label |> Set.to_list
    in
    let new_exit_instr =
      block.exit
      |> Control_instr.map_block_calls ~f:(fun block_call ->
        { block_call with args = Map.find_exn liveness block_call.label |> Set.to_list })
    in
    { block with entry = new_entry_instr; exit = new_exit_instr }
  in
  let function_with_block_args =
    (Field.map Function.Fields.graph & Cfg_graph.map_simple_order)
      ~f:(fun (label, block) -> add_block_args_and_calls label block)
      fn
  in
  let renamed_function = Rename.rename_function function_with_block_args in
  validate_ssa_function renamed_function |> Or_error.ok_exn;
  renamed_function
;;

let get_phis (graph : Vir.Graph.t) =
  let initial_phis =
    F.Core.Map.mapi graph.blocks ~f:(fun (label, block) ->
      block.entry
      |> List.map ~f:(fun arg : _ Phi.t ->
        { dest_label = label; dest = arg; flow_values = [] }))
  in
  let fold =
    F.Fold.(F.Core.Map.foldi @> ix (of_fn Block.exit @> Control_instr.block_calls_fold))
  in
  let phis_of_block =
    F.Fold.fold
      fold
      ~init:initial_phis
      ~f:(fun phi_map (label, (block_call : Vir.Block_call.t)) ->
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
  (* todo, only need to create a unification variable for each phi dest, not every value *)
  (* some PhiValue s will not be actuall phi destinations, therefore cannot change *)
  let subst = Value.Hashtbl.create () in
  let phis_with_id : value_id Phi.t list =
    (List.map & Phi.map)
      ~f:(fun value ->
        Hashtbl.find_or_add subst value ~default:(fun () -> Union_find.create value))
      phis
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
    List.all_equal other_values ~equal:[%equal: ValueWithId.t PhiValue.t]
  in
  (* todo: use more efficient bag data structure *)
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
         (* the phi variable is not consed back on here, we removed it *)
         `Removed, phis)
  in
  let rec fixpoint phis =
    match try_remove phis with
    | `Didn'tRemove, phis -> phis
    | `Removed, phis -> fixpoint phis
  in
  let simplified_phis = fixpoint phis_with_id |> List.map ~f:zonk_phi_final in
  let zonked_subst = Hashtbl.map subst ~f:Union_find.get in
  zonked_subst, simplified_phis
;;

let put_phis (phis : Value.t Phi.t list) (graph : Vir.Graph.t) =
  let phis_of_label = Hashtbl.create (module Label) in
  List.iter phis ~f:(fun phi ->
    Hashtbl.update
      phis_of_label
      phi.dest_label
      ~f:(Option.value_map ~default:[ phi ] ~f:(List.cons phi)));
  (Field.map Cfg_graph.Fields.blocks & F.Core.Map.mapi) graph ~f:(fun (label, block) ->
    let phis = Hashtbl.find phis_of_label label |> Option.value ~default:[] in
    let entry = List.map phis ~f:(fun phi -> phi.dest) in
    let exit =
      Control_instr.map_block_calls block.exit ~f:(fun block_call ->
        let phis_for_call =
          Hashtbl.find phis_of_label block_call.label |> Option.value ~default:[]
        in
        let call_args =
          List.map phis_for_call ~f:(fun phi ->
            let flow_value =
              List.find_exn phi.flow_values ~f:(fun flow_value ->
                [%equal: Label.t] label flow_value.flowed_from_label)
            in
            flow_value.value)
        in
        { block_call with args = call_args })
    in
    { block with entry; exit })
;;

let subst_graph subst (graph : Vir.Graph.t) =
  (Field.map Cfg_graph.Fields.blocks & F.Core.Map.map) graph ~f:(fun block ->
    Block.map_instrs_forwards
      { f =
          (fun instr ->
            Generic_instr.map_uses instr ~f:(fun use ->
              Hashtbl.find subst use |> Option.value ~default:use))
      }
      block)
;;

let convert_ssa_function (fn : Vir.Function.t) =
  let fn = convert_naive_ssa fn in
  let phis = get_phis fn.graph in
  let subst, simplified_phis =
    F.Core.Map.fold @> F.Core.List.fold
    |> (fun f -> F.Fold.reduce f F.Reduce.to_list_rev phis)
    |> simplify_phis
  in
  let graph = subst_graph subst fn.graph in
  let graph = put_phis simplified_phis graph in
  { fn with graph }
;;

let convert_ssa (program : Vir.Program.t) =
  let program =
    (Field.map Program.Fields.functions & List.map) ~f:convert_ssa_function program
  in
  validate_ssa program;
  program
;;
