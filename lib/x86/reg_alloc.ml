open O
open Types
open Utils.Instr_types
module Interference = Compiler.Reg_alloc.Interference
module Reg_alloc = Compiler.Reg_alloc

module Ra = Reg_alloc.Make (struct
    module Register = struct
      include Mach_reg

      let order =
        [ (* callee saved *)
          RAX
        ; RDI
        ; RSI
        ; RDX
        ; RCX
        ; R8
        ; R9
        ; R10
        ; (* 11 used for scratch register *)
          (* caller saved *)
          RBX
        ; R12
        ; R13
        ; R14
        ; R15
        ]
      ;;
    end

    module RegisterSet = struct
      type t = Register.t Hash_set.t [@@deriving sexp_of]
      type elt = Register.t

      let create () = Hash_set.create (module Register)
      let mem = Hash_set.mem
      let add = Hash_set.add
    end
  end)

module NameMap = Entity.Map.Make (Name)

let transfer = Dataflow.Liveness.instr_transfer |> Cfg.Dataflow.Instr_transfer.transfer

let add_block_edges interference block live_out =
  let live_out = ref live_out in
  Block.instrs_backward_fold block (fun instr ->
    let without =
      match Instr.to_variant instr with
      | Instr_variant.Real (MInstr.Mov { src = Operand.Reg src; _ }) -> [ src ]
      | _ -> []
    in
    let is_edge def live =
      (* don't add an edge to itself *)
      (not ([%equal: VReg.t] def live))
      (* don't add an edge to something that is a register mov *)
      (* we want these to be allocated to the same register, so we can remove the redundant mov *)
      && not (List.mem without live ~equal:[%equal: VReg.t])
    in
    (* make sure we at least add the defs in, because the register allocator uses the domain of interference as all nodes *)
    Instr.defs_fold instr
    |> F.Iter.iter ~f:(fun def -> Interference.add_node interference def.VReg.name);
    (* add interference edges *)
    F.Iter.(
      product (Instr.defs_fold instr) (FC.Set.iter !live_out)
      |> filter ~f:(fun (def, live) -> is_edge def live)
      |> iter ~f:(fun (def, live) ->
        Interference.add_edge interference def.VReg.name live.VReg.name;
        ()));
    live_out := transfer instr !live_out;
    ());
  ()
;;

let collect_precolored fn =
  F.Fold.(
    Function.instrs_forward_fold
    @> Instr.regs_fold
    @> of_fn (fun (vreg : VReg.t) ->
      vreg.precolored |> Option.map ~f:(fun reg -> `name vreg.name, `reg reg))
    @> FC.Option.fold)
    fn
  |> F.Iter.map ~f:(fun (`name name, `reg reg) -> name, reg)
  |> NameMap.of_iter
;;

let construct_fn fn =
  let _, live_out_facts = Dataflow.Liveness.run fn in
  let interference = Interference.create () in
  (Cfg.Graph.to_iteri fn.graph) (fun (label, block) ->
    let live_out = Cfg.Dataflow.Fact_base.find_exn live_out_facts label in
    add_block_edges interference block live_out);
  interference
;;

let alloc_fn fn =
  let open Result.Let_syntax in
  let interference = construct_fn fn in
  let precolored = collect_precolored fn in
  let%bind allocation = Ra.Greedy.run ~precolored ~interference in
  Ok allocation
;;

let collect_all_vregs fn =
  (Function.instrs_forward_fold @> Instr.regs_fold) fn
  |> F.Iter.map ~f:(fun (reg : VReg.t) -> reg.name, reg)
  |> NameMap.of_iter
;;

type context =
  { instrs : (MReg.t Instr.t, read_write) Vec.t (* ; stack_layout : Stack_layout.t *)
  ; mutable unique_name : Name.Id.t
  ; spill_slots : (Mach_reg.t, Name.t) Hashtbl.t
  }

module Cx = struct
  let create fn =
    { instrs = Vec.create ()
    ; unique_name = fn.Function.unique_name
    ; spill_slots = Hashtbl.create (module Mach_reg)
    }
  ;;

  let add cx = Vec.push cx.instrs

  let fresh_name cx s =
    let name = Name.create s cx.unique_name in
    cx.unique_name <- Name.Id.next cx.unique_name;
    name
  ;;

  let add_vinstr cx vinstr = Vec.push cx.instrs (Instr.Virt vinstr)

  let get_spill_slot cx reg =
    match Hashtbl.find cx.spill_slots reg with
    | Some name -> name
    | None ->
      let name = fresh_name cx "spill_mach_reg" in
      Hashtbl.add_exn cx.spill_slots ~key:reg ~data:name;
      add_vinstr cx @@ ReserveStackLocal { name; size = 8l };
      name
  ;;

  let add_minstr cx minstr = Vec.push cx.instrs (Instr.Real minstr)
end

let lower_minstr cx minstr =
  let module Set = Mach_reg_set in
  let module Enum_set = Data.Enum_set in
  (* find registers that weren't used by the instruction *)
  let set =
    let set = Set.create () in
    MInstr.regs_fold minstr
    |> F.Iter.filter_map ~f:AReg.reg_val
    |> F.Iter.iter ~f:(fun reg -> Set.add set reg);
    Enum_set.negate set;
    Set.remove set R11;
    set
  in
  let rs = (fun f -> Set.iter set ~f) |> Iter.to_list in
  let spilled =
    MInstr.regs_fold minstr
    |> F.Iter.filter_map ~f:(fun areg ->
      match areg with
      | AReg.Spilled { name; _ } -> Some name
      | _ -> None)
    |> F.Iter.to_list
  in
  assert (List.length spilled <= 9);
  let reg_of_spilled_list, unzipped = List.zip_with_remainder spilled rs in
  (match unzipped with
   | Some (Second _) -> ()
   | Some (First _) | None ->
     raise_s
       [%message
         "impossible, list of registers must be greater than number of spilled because \
          we have only 9 maximum spills"]);
  let used_registers = List.map ~f:snd reg_of_spilled_list in
  (* save used registers *)
  List.iter used_registers ~f:(fun reg ->
    let spill_slot = Cx.get_spill_slot cx reg in
    Cx.add_minstr cx
    @@ Mov
         { s = Q
         ; dst = Mem (Address.stack_local spill_slot)
         ; src = Reg (MReg.create ~name:spill_slot.name Q reg)
         });
  let reg_of_spilled =
    F.Iter.of_list reg_of_spilled_list |> FC.Hashtbl.of_iter (module Name)
  in
  (* move spilled uses to registers *)
  MInstr.uses_fold minstr
  |> F.Iter.filter_map ~f:AReg.spilled_val
  |> F.Iter.iter ~f:(fun (`s s, `name spilled) ->
    let reg = Hashtbl.find_exn reg_of_spilled spilled in
    Cx.add_minstr cx
    @@ Mov
         { s
         ; dst = Reg (MReg.create ~name:spilled.name s reg)
         ; src = Mem (Address.stack_local spilled)
         });
  (* use the registers instead of the stack slots *)
  let new_minstr =
    MInstr.map_regs minstr ~f:(fun areg ->
      match areg with
      | Spilled { s; name } ->
        MReg.create ~name:name.name s (Hashtbl.find_exn reg_of_spilled name)
      | InReg { s; name; reg } -> MReg.create ~name s reg)
  in
  Cx.add_minstr cx new_minstr;
  (* move defined registers to spilled *)
  MInstr.defs_fold minstr (fun def ->
    match def with
    | Spilled { s; name } ->
      let spilled_reg = Hashtbl.find_exn reg_of_spilled name in
      Cx.add_minstr cx
      @@ Mov
           { s
           ; dst = Mem (Address.stack_local name)
           ; src = Reg (MReg.create ~name:name.name s spilled_reg)
           };
      ()
    | _ -> ());
  (* restore registers *)
  List.iter used_registers ~f:(fun reg ->
    let spill_slot = Cx.get_spill_slot cx reg in
    Cx.add_minstr cx
    @@ Mov
         { s = Q
         ; dst = Reg (MReg.create Q reg)
         ; src = Mem (Address.stack_local spill_slot)
         })
;;

let lower_vinstr cx (instr : AReg.t VInstr.t) =
  match instr with
  | Par_mov movs ->
    let module W = Compiler.Windmills in
    let movs = List.map movs ~f:(fun (dst, src) -> W.Move.create ~dst ~src) in
    let movs, _did_use_scratch =
      W.convert
        ~eq:AReg.equal
        ~scratch:(fun reg ->
          AReg.InReg { s = AReg.size reg; name = "par_mov_scratch"; reg = R11 })
        movs
    in
    let movs =
      List.map movs ~f:(fun { dst; src } ->
        MInstr.Mov { s = AReg.size dst; dst = Reg dst; src = Reg src })
    in
    List.iter movs ~f:(lower_minstr cx);
    ()
  | Def _ | ReserveStackEnd _ | ReserveStackLocal _ -> ()
  | Block_args _ -> raise_s [%message "should have been remove by remove_ssa"]
;;

let lower_block_call { Block_call.label; _ } = { Block_call.label; args = [] }

let lower_jump = function
  | Jump.Jump j -> Jump.Jump (lower_block_call j)
  | Jump.CondJump { cond; j1; j2 } ->
    Jump.CondJump { cond; j1 = lower_block_call j1; j2 = lower_block_call j2 }
;;

let lower_instr cx instr =
  match instr with
  | Instr.Virt vinstr -> lower_vinstr cx vinstr
  | Instr.Real minstr -> lower_minstr cx minstr
  | Instr.Jump jump -> Cx.add cx @@ Instr.Jump (lower_jump jump)
;;

let lower_block cx (block : _ Block.t) =
  (Block.instrs_forward_fold block) (lower_instr cx);
  let instrs = Vec.copy_exact cx.instrs in
  Vec.clear cx.instrs;
  { Block.instrs }
;;

let lower_function fn =
  let cx = Cx.create fn in
  let fn = Function.map_blocks fn ~f:(lower_block cx) in
  { fn with unique_name = cx.unique_name }
;;

let map_last vec ~f =
  let vec = Vec.copy_exact ~size:(Vec.length vec + 1) vec in
  Vec.set vec (Vec.length vec - 1) (f (Vec.get vec (Vec.length vec - 1)));
  Vec.freeze vec
;;

let apply_allocation_instr ~allocation (instr : VReg.t Instr.t) =
  let spilled = Vec.create () in
  ( spilled |> Vec.freeze
  , Instr.map_regs instr ~f:(fun vreg : AReg.t ->
      match Ra.Allocation.find_exn allocation vreg.name with
      | Spilled ->
        Vec.push spilled (vreg.s, vreg.name);
        Spilled { s = vreg.s; name = vreg.name }
      | InReg reg -> InReg { s = vreg.s; name = vreg.name.name; reg }) )
;;

let apply_allocation_block ~allocation (block : VReg.t Block.t) =
  let instrs = Vec.create ~size:(Vec.length block.instrs) () in
  Vec.iter block.instrs ~f:(fun instr ->
    let spilled, instr = apply_allocation_instr ~allocation instr in
    Vec.iter spilled ~f:(fun (s, name) ->
      Vec.push instrs
      @@ Instr.Virt
           (ReserveStackLocal { name; size = Size.to_byte_size s |> Int32.of_int_exn }));
    Vec.push instrs instr);
  { Block.instrs = Vec.freeze instrs }
;;

let apply_allocation ~allocation (fn : VReg.t Function.t) =
  Function.map_blocks fn ~f:(apply_allocation_block ~allocation)
;;

let remove_ssa (fn : _ Function.t) =
  let graph = fn.graph in
  let graph =
    Cfg.Graph.foldi graph ~init:graph ~f:(fun graph (label, block) ->
      let jump = Block.get_jump block in
      let jump_with_no_args =
        Jump.map_block_calls jump ~f:(fun j -> { j with args = [] })
      in
      let block_calls = Jump.block_calls_fold jump |> F.Iter.to_list in
      let get_data (j : _ Block_call.t) =
        let args = j.args in
        let to_block = Cfg.Graph.find_exn j.label graph in
        let params = to_block |> Block.get_block_args in
        let par_mov = List.zip_exn params args |> VInstr.Par_mov in
        to_block, par_mov
      in
      match block_calls with
      | [ j ] ->
        let to_block, par_mov = get_data j in
        let instrs =
          block.instrs |> fun vec -> Vec.copy_exact ~size:(Vec.length vec + 1) vec
        in
        let _ = Vec.pop_exn instrs in
        Vec.push instrs @@ Instr.Virt par_mov;
        Vec.push instrs @@ Jump jump_with_no_args;
        let instrs = Vec.freeze instrs in
        graph
        |> Cfg.Graph.set label { Block.instrs }
        |> Cfg.Graph.set j.label (Block.replace_first (Real NoOp) to_block)
      | js ->
        List.fold js ~init:graph ~f:(fun graph j ->
          let to_block, par_mov = get_data j in
          let block =
            { Block.instrs = block.instrs |> map_last ~f:(fun _ -> Jump jump_with_no_args)
            }
          in
          graph
          |> Cfg.Graph.set label block
          |> Cfg.Graph.set label (Block.replace_first (Virt par_mov) to_block)))
  in
  { fn with graph }
;;
