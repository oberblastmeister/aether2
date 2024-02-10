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
      type value = Register.t

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
  let%bind allocation = Ra.run ~precolored ~interference in
  Ok allocation
;;

let collect_all_vregs fn =
  (Function.instrs_forward_fold @> Instr.regs_fold) fn
  |> F.Iter.map ~f:(fun (reg : VReg.t) -> reg.name, reg)
  |> NameMap.of_iter
;;

module Stack_layout : sig
  type t [@@deriving sexp_of]

  val create
    :  vregs:(Name.t, VReg.t) Entity.Map.t
    -> allocation:Ra.Allocation.t
    -> VReg.t Function.t
    -> t

  val end_offset : t -> int32 -> int32
  val local_offset : t -> Name.t -> int32
  val spilled_offset : t -> Name.t -> int32
end = struct
  type t =
    { stack_size : int32
    ; offset_of_local : (Name.t, int32) Hashtbl.t
    ; locals_offset : int32
    ; end_offset : int32
    ; spilled_offset : int32
    ; offset_of_spilled : (Name.t, int32) Hashtbl.t
    }
  [@@deriving sexp_of]

  open Int32

  let end_offset t i = t.end_offset + i
  let local_offset t name = t.locals_offset + Hashtbl.find_exn t.offset_of_local name
  let spilled_offset t name = t.spilled_offset + Hashtbl.find_exn t.offset_of_spilled name

  let create ~vregs ~allocation fn =
    let locals_size = ref 0l in
    let offset_of_local = Hashtbl.create (module Name) in
    let end_size = ref 0l in
    F.Fold.(Function.instrs_forward_fold @> of_fn Instr.get_virt @> FC.Option.fold)
      fn
      (fun instr ->
         (match instr with
          | VInstr.ReserveStackEnd { size } -> end_size := max !end_size size
          | VInstr.ReserveStackLocal { name; size } ->
            Hashtbl.add_exn offset_of_local ~key:name ~data:!locals_size;
            locals_size := !locals_size + size;
            ()
          | _ -> ());
         ());
    let offset_of_spilled = Hashtbl.create (module Name) in
    let spilled_size = ref 0l in
    Ra.Allocation.to_spilled_iter allocation
    |> F.Iter.iter ~f:(fun spilled_name ->
      let vreg = NameMap.find_exn vregs spilled_name in
      let size =
        Size.to_byte_size vreg.VReg.s |> of_int_exn |> round_up ~to_multiple_of:8l
      in
      Hashtbl.add_exn offset_of_spilled ~key:spilled_name ~data:!spilled_size;
      spilled_size := !spilled_size + size);
    let align size =
      assert (size % 8l = 0l);
      if size % 16l = 8l then size + 8l else size
    in
    (* TODO: align stack, align stuff *)
    { stack_size = align @@ (!locals_size + !spilled_size + !end_size)
    ; end_offset = 0l
    ; locals_offset = !end_size
    ; offset_of_local
    ; spilled_offset = !locals_size + !end_size
    ; offset_of_spilled
    }
  ;;
end

type context =
  { allocation : Ra.Allocation.t
  ; instrs : (VReg.t Instr.t, write) Vec.t (* ; stack_layout : Stack_layout.t *)
  ; mutable unique_name : Name.Id.t
  ; temp_name : Name.t
  }

module Cx = struct
  let r11 cx s = VReg.precolored s cx.temp_name R11

  let fresh_name cx s =
    let name = Name.create s cx.unique_name in
    cx.unique_name <- Name.Id.next cx.unique_name;
    name
  ;;

  let is_spilled cx vreg =
    match Ra.Allocation.find_exn cx.allocation vreg.VReg.name with
    | Spilled -> true
    | InReg _ -> false
  ;;

  let apply_vreg cx (vreg : VReg.t) =
    match Ra.Allocation.find_exn cx.allocation vreg.name with
    | Spilled -> Operand.stack_local vreg.name
    | InReg reg -> Operand.precolored vreg.s vreg.name reg
  ;;

  let add_minstr cx minstr = Vec.push cx.instrs (Instr.Real minstr)
end

(* precondition, mov must be legalized*)
(* let lower_stack_off ~cx off =
  let open Stack_off in
  match off with
  | End i -> Stack_layout.end_offset cx.stack_layout i
  | Local name -> Stack_layout.local_offset cx.stack_layout name
;;

let lower_imm ~cx (imm : VReg.t Imm.t) : Mach_reg.t Imm.t =
  let open Imm in
  match imm with
  | Int i -> Int i
  | Stack off -> Int (lower_stack_off ~cx off)
;;

let lower_address ~cx ~can_use_scratch address =
  let open Address in
  let vreg_is_spilled reg =
    match Ra.Allocation.find_exn cx.allocation reg.VReg.name with
    | Spilled -> true
    | InReg _ -> false
  in
  let base_is_spilled (base : _ Base.t) =
    match base with
    | Reg reg -> vreg_is_spilled reg
    | None | Rip -> false
  in
  let index_is_spilled (index : _ Index.t) =
    match index with
    | None -> false
    | Some { index; _ } -> vreg_is_spilled index
  in
  let move_base_scratch (base : _ Base.t) =
    match base with
    | Reg reg -> ()
    | None | Rip -> ()
  in
  (* match address with
  | Imm { offset; scale } -> Imm { offset = lower_imm ~cx offset; scale }
  | Complex { base; index; offset }
    when base_is_spilled base && index_is_spilled index && can_use_scratch ->
    
       ()
  | Complex { base; index; offset } -> () *)
  ()
;; *)

(* if can_use_scratch then (

   ) else () *)

let apply_allocation_vinstr ~cx instr =
  let open VInstr in
  let open MInstr in
  match instr with
  | Par_mov movs ->
    let module W = Compiler.Windmills in
    let scratch_name = Cx.fresh_name cx "scratch" in
    let get_size = function
      | First vreg -> vreg.VReg.s
      | Second s -> s
    in
    let get_name = function
      | First vreg -> vreg.VReg.name
      | Second _ -> scratch_name
    in
    let convert movs =
      let movs =
        List.map movs ~f:(fun (dst, src) ->
          W.Move.create ~dst:(First dst) ~src:(First src))
      in
      W.convert ~get_name ~scratch:(fun reg -> Second (get_size reg)) movs
    in
    let had_spilled_mov =
      List.exists movs ~f:(fun (dst, src) -> Cx.is_spilled cx dst && Cx.is_spilled cx src)
    in
    let movs, _ = convert movs in
    let movs =
      List.map movs ~f:(fun ({ dst; src } : _ W.Move.t) ->
        let to_operand = function
          | First vreg -> Operand.Reg vreg
          | Second _ when had_spilled_mov -> Operand.stack_local scratch_name
          | Second s -> Operand.Reg (VReg.precolored s scratch_name R11)
        in
        Mov { s = get_size dst; dst = to_operand dst; src = to_operand src })
    in
    List.iter movs ~f:(Cx.add_minstr cx);
    ()
  | Def _ | ReserveStackEnd _ | ReserveStackLocal _ -> ()
  | Block_args _ -> raise_s [%message "should have been remove by remove_ssa"]
;;

let apply_allocation_address ~cx size address =
  let r11 = Cx.r11 cx size in
  match address with
  | Address.Complex { base; index; offset } ->
    let stack_slot_address = Cx.fresh_name cx "spill_address" |> Address.stack_local in
    let stack_slot = Operand.Mem stack_slot_address in
    (match base with
     | None ->
       Cx.add_minstr cx @@ Mov { s = size; dst = Reg r11; src = Imm offset };
       ()
     | Reg r ->
       Cx.add_minstr cx @@ Mov { s = size; dst = Reg r11; src = Cx.apply_vreg cx r };
       Cx.add_minstr cx
       @@ Add { s = size; dst = stack_slot; src1 = Reg r11; src2 = Imm offset };
       ()
     | Rip ->
       (* need to do this with the offset *)
       Cx.add_minstr cx
       @@ Mov { s = size; dst = Reg r11; src = Operand.Mem (Address.rip_relative offset) };
       ());
    Cx.add_minstr cx @@ Mov { s = size; dst = stack_slot; src = Reg r11 };
    (match index with
     | None -> ()
     | Some { index; scale } ->
       Cx.add_minstr cx
       @@ Mov { s = size; dst = Operand.Reg r11; src = Cx.apply_vreg cx index };
       Cx.add_minstr cx
       @@ Lea { s = size; dst = r11; src = Address.index_scale r11 scale };
       Cx.add_minstr cx
       @@ MInstr.Add { s = size; dst = stack_slot; src1 = stack_slot; src2 = Reg r11 };
       ());
    Some stack_slot_address
  | Address.Imm _ -> None
;;

let apply_allocation_operand ~cx size operand =
  let r11 = Cx.r11 cx size in
  match operand with
  | Operand.Reg reg when Cx.is_spilled cx reg -> Operand.stack_local reg.name
  | Operand.Mem address
    when Address.regs_fold address |> F.Iter.exists ~f:(Cx.is_spilled cx) ->
    let stack_slot = Cx.fresh_name cx "spill_address" |> Address.stack_local in
    let address_of_address =
      apply_allocation_address ~cx size address |> Option.value_exn
    in
    Cx.add_minstr cx
    @@ Mov { s = size; dst = Mem stack_slot; src = Mem address_of_address };
    Mem stack_slot
  | Operand.Mem address -> todo ()
  | Operand.Reg _ | Operand.Imm _ -> operand
;;

let apply_allocation_minstr minstr =
  match minstr with
  | _ -> MInstr.operands_fold minstr (fun o -> ())
;;

(* MInstr.operands_fold minstr ~on_def:(fun o ->
   ) *)
(* match minstr with
   | _ -> () *)

let apply_allocation_block ~cx ~allocation (block : _ Block.t) =
  let instrs = Vec.create () in
  (Block.instrs_forward_fold block) (fun instr -> ());
  ()
;;

let map_last vec ~f =
  let vec = Vec.copy_exact ~size:(Vec.length vec + 1) vec in
  Vec.set vec (Vec.length vec - 1) (f (Vec.get vec (Vec.length vec - 1)));
  Vec.freeze vec
;;

let remove_ssa (fn : VReg.t Function.t) =
  let graph = fn.graph in
  let graph =
    Cfg.Graph.foldi graph ~init:graph ~f:(fun graph (label, block) ->
      let jump = Block.get_jump block in
      let jump_with_no_args =
        Jump.map_block_calls jump ~f:(fun j -> { j with args = Block_call [] })
      in
      let block_calls = Jump.block_calls_fold jump |> F.Iter.to_list in
      let get_data (j : _ Block_call.t) =
        let args = Maybe_block_call.get_args j.args in
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
  { Function.graph }
;;
