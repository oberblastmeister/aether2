open! O
open Utils.Instr_types
module T = Types_basic
module Name = Name
module Label = Label
module Control = Control

module Cond = struct
  include T.Cond
end

module Mach_reg = struct
  include T.Mach_reg
end

module Mach_reg_set = Data.Enum_set.Make (Mach_reg)

module MReg = struct
  include T.MReg
end

module AReg = struct
  include T.AReg

  let reg_val = function
    | InReg { reg; _ } -> Some reg
    | _ -> None
  ;;

  let size (Spilled { s; _ } | InReg { s; _ }) = s

  (* let in_reg s name reg = { s; name; reg = Some reg }
  let spilled s name = { s; name; reg = None }
  let is_spilled t = Option.is_none t.reg
  let is_in_reg t = Option.is_some t.reg

  let to_spilled t =
    match t.reg with
    | None -> Some (t.s, t.name)
    | Some _ -> None
  ;; *)
end

module VReg = struct
  include T.VReg

  let to_name r = r.name
  let create s name = { s; name; precolored = None }
  let precolored s name precolored = { s; name; precolored = Some precolored }
end

module Size = struct
  include T.Size

  let to_byte_size = function
    | Q -> 8
    | L -> 4
    | W -> 2
    | B -> 1
  ;;
end

module Stack_off = struct
  include T.Stack_off
end

module Address = struct
  include T.Address

  module Base = struct
    include Base

    let regs_fold a k = reg_val a |> Option.iter ~f:k
  end

  module Scale = struct
    include Scale
  end

  module Index = struct
    include Index

    let regs_fold a k =
      some_val a |> Option.iter ~f:(fun (`index index, `scale _) -> k index)
    ;;
  end

  let stack_local name = Imm { offset = Stack (Local name); scale = One }
  let base base = Complex { base; index = None; offset = Int 0l }

  let index_scale index scale =
    Complex { base = None; index = Some { index; scale }; offset = Int 0l }
  ;;

  let base_offset base offset = Complex { base; index = None; offset }
  let rip_relative offset = base_offset Base.Rip offset

  let regs_fold a k =
    match a with
    | Imm _ -> ()
    | Complex { base; index; _ } ->
      Base.regs_fold base k;
      Index.regs_fold index k
  ;;
end

module Imm = struct
  include T.Imm
end

module Operand = struct
  include T.Operand

  let imm i = Imm (Imm.Int i)
  let stack_off_end i = Imm (Imm.Stack (Stack_off.End i))
  let stack_local name = Mem (Address.stack_local name)
  let vreg s name = Reg (VReg.create s name)
  let precolored s name precolored = Reg (VReg.precolored s name precolored)
  let reg_val_fold o k = reg_val o |> Option.iter ~f:k
  let mem_val_fold o k = mem_val o |> Option.iter ~f:k
  let mem_regs_fold o k = (mem_val_fold @> Address.regs_fold) o k

  let any_regs_fold o k =
    match o with
    | Reg r -> k r
    | Mem m -> Address.regs_fold m k
    | Imm _ -> ()
  ;;

  let of_areg = function
    | AReg.InReg { name; reg; s } -> Reg (MReg.create ~name s reg)
    | AReg.Spilled { name; _ } -> stack_local name
  ;;
end

module Block_call = struct
  include T.Block_call

  let uses_fold (type r) (instr : r t) (k : r -> unit) = instr.args |> List.iter ~f:k
  let regs_fold block_call k = uses_fold block_call k
  let map_regs i ~f = map f i
end

module Jump = struct
  include T.Jump

  let map_regs i ~f = map f i

  let regs_fold i k =
    match i with
    | Jump j -> Block_call.regs_fold j k
    | CondJump { j1; j2; _ } ->
      Block_call.regs_fold j1 k;
      Block_call.regs_fold j2 k
    | Ret -> ()
  ;;

  let uses_fold i k =
    match i with
    | Jump j -> Block_call.uses_fold j k
    | CondJump { j1; j2; _ } ->
      Block_call.uses_fold j1 k;
      Block_call.uses_fold j2 k
    | Ret -> ()
  ;;

  let block_calls_fold j k =
    match j with
    | Jump j -> k j
    | CondJump { j1; j2; _ } ->
      k j1;
      k j2
    | Ret -> ()
  ;;

  let map_block_calls j ~f =
    match j with
    | Jump j -> Jump (f j)
    | CondJump { j1; j2; cond } -> CondJump { j1 = f j1; j2 = f j2; cond }
    | Ret -> Ret
  ;;

  let map_regs i = (map_block_calls & Block_call.map_regs) i
end

module VInstr = struct
  include T.VInstr

  let map_regs i ~f = map f i

  let regs_fold i k =
    let module O = Operand in
    let module A = Address in
    match i with
    | Par_mov movs -> (FC.List.fold @> FC.Tuple2.fold_both) movs k
    | Def { dst; _ } -> k dst
    | Block_args regs -> FC.List.fold regs k
    | ReserveStackEnd _ | ReserveStackLocal _ -> ()
  ;;

  let defs_fold i k =
    let module O = Operand in
    match i with
    | Def { dst; _ } -> k dst
    | Par_mov movs -> (FC.List.fold @> F.Fold.of_fn fst) movs k
    | Block_args args -> FC.List.fold args k
    | ReserveStackEnd _ | ReserveStackLocal _ -> ()
  ;;

  let uses_fold i k =
    let module O = Operand in
    match i with
    | Par_mov movs -> (FC.List.fold @> F.Fold.of_fn snd) movs k
    | Block_args _ -> ()
    | Def _ | ReserveStackEnd _ | ReserveStackLocal _ -> ()
  ;;
end

module GOperand = T.GOperand

module MInstr = struct
  include T.MInstr

  let operands_fold_with i ~on_def ~on_use =
    let module O = Operand in
    let module A = Address in
    match i with
    | NoOp -> ()
    | Lea { dst; src; _ } ->
      on_def @@ O.Reg dst;
      on_def @@ O.Reg dst;
      on_use @@ O.Mem src
    | Add { dst; src1; src2; _ } ->
      on_def dst;
      on_use src1;
      on_use src2
    | MovAbs { dst; _ } -> on_def dst
    | Mov { dst; src; _ } ->
      on_def dst;
      on_use src
    | Cmp { src1; src2; _ } | Test { src1; src2; _ } ->
      on_use src1;
      on_use src2
    | Set { dst; _ } -> on_def dst
    | Push { src; _ } -> on_use @@ O.Reg src
    | Pop { dst; _ } -> on_def @@ O.Reg dst
  ;;

  type 'r mapper = { f : 'op. ('r, 'op) GOperand.t -> ('r, 'op) GOperand.t }

  let map_operands i ~f:{ f = gmap } =
    let map_op (o : 'a Operand.t) =
      match o with
      | Operand.Reg r -> GOperand.to_operand (gmap (Reg r))
      | Mem m -> GOperand.to_operand (gmap (Mem m))
      | Imm i -> GOperand.to_operand (gmap (Imm i))
    in
    let map_address a = gmap (Mem a) |> GOperand.mem_val in
    let map_imm i = gmap (Imm i) |> GOperand.imm_val in
    let map_reg r = gmap (Reg r) |> GOperand.reg_val in
    match i with
    | NoOp -> NoOp
    | Mov { s; dst; src } -> Mov { s; dst = map_op dst; src = map_op src }
    | Lea { dst; src; s } -> Lea { dst = map_reg dst; src = map_address src; s }
    | Add { dst; src1; src2; s } ->
      Add { dst = map_op dst; src1 = map_op src1; src2 = map_op src2; s }
    | Push { src; s } -> Push { src = map_reg src; s }
    | Pop { dst; s } -> Pop { dst = map_reg dst; s }
    | MovAbs { dst; imm } -> MovAbs { dst = map_op dst; imm }
    | Cmp { s; src1; src2 } -> Cmp { s; src1 = map_op src1; src2 = map_op src2 }
    | Test { s; src1; src2; _ } -> Test { s; src1 = map_op src1; src2 = map_op src2 }
    | Set { s; dst; cond } -> Set { s; dst = map_op dst; cond }
  ;;

  let operands_fold i k = operands_fold_with i ~on_def:k ~on_use:k
  let regs_fold i k = operands_fold i (fun o -> Operand.any_regs_fold o k)
  let map_regs i ~f = map f i

  let defs_fold i k =
    operands_fold_with i ~on_def:(fun o -> Operand.reg_val_fold o k) ~on_use:(Fn.const ())
  ;;

  let uses_fold i k =
    operands_fold_with
      i
      ~on_def:(fun o -> Operand.mem_regs_fold o k)
      ~on_use:(fun o -> Operand.any_regs_fold o k)
  ;;
end

module Instr_variant = struct
  include T.Instr_variant
end

module Instr = struct
  include T.Instr

  let to_variant (type r) (i : r t) =
    match i with
    | Virt v -> Instr_variant.Virt v
    | Real r -> Instr_variant.Real r
    | Jump j -> Instr_variant.Jump j
  ;;

  let map_regs i ~f =
    match i with
    | Virt i -> Virt (VInstr.map_regs i ~f)
    | Real i -> Real (MInstr.map_regs i ~f)
    | Jump i -> Jump (Jump.map_regs i ~f)
  ;;

  let regs_fold (type r) (i : r t) (k : r -> unit) =
    let module O = Operand in
    let module A = Address in
    match i with
    | Virt i -> VInstr.regs_fold i k
    | Real i -> MInstr.regs_fold i k
    | Jump i -> Jump.regs_fold i k
  ;;

  let defs_fold i k =
    let module O = Operand in
    let open Instr_variant in
    match to_variant i with
    | Virt i -> VInstr.defs_fold i k
    | Real i -> MInstr.defs_fold i k
    | Jump _ -> ()
  ;;

  let uses_fold (type r) (i : r t) (k : r -> unit) =
    let module O = Operand in
    let open Instr_variant in
    match i with
    | Virt i -> VInstr.uses_fold i k
    | Real i -> MInstr.uses_fold i k
    | Jump i -> Jump.uses_fold i k
  ;;

  let mov_to_reg_from_stack s reg (stack_name : Name.t) =
    Real
      (Mov
         { s
         ; dst = Reg (MReg.create ~name:stack_name.name s reg)
         ; src = Mem (Address.stack_local stack_name)
         })
  ;;

  let mov_to_stack_from_reg s (stack_name : Name.t) reg =
    Real
      (Mov
         { s
         ; dst = Mem (Address.stack_local stack_name)
         ; src = Reg (MReg.create ~name:stack_name.name s reg)
         })
  ;;
end

module Block = struct
  include T.Block

  let first_instr b = Vec.first b.instrs

  let jump_exn block =
    Vec.last block.instrs
    |> Option.value_or_thunk ~default:(fun () ->
      raise_s [%message "block must have a last instr"])
    |> Instr.to_variant
    |> fun instr ->
    Instr_variant.jump_val instr
    |> Option.value_or_thunk ~default:(fun () ->
      raise_s [%message "last instr must be a jump" (instr : _ Instr_variant.t)])
  ;;

  let block_args_exn block =
    Vec.first block.instrs
    |> Option.value_or_thunk ~default:(fun () ->
      raise_s [%message "block must have a first instr"])
    |> fun instr ->
    instr
    |> Instr.to_variant
    |> fun instr ->
    (let open Option.Let_syntax in
     let%bind virt = Instr_variant.virt_val instr in
     let%bind block_args = VInstr.block_args_val virt in
     return block_args)
    |> Option.value_or_thunk ~default:(fun () ->
      raise_s
        [%message "first instruction must be block arguments" (instr : _ Instr_variant.t)])
  ;;

  let cons instr b = { instrs = Vec.cons instr b.instrs }

  let jumps_fold block k =
    match jump_exn block with
    | Jump j -> k j.label
    | CondJump { j1; j2; _ } ->
      k j1.label;
      k j2.label
    | Ret -> ()
  ;;

  let instrs_forward_fold b = Vec.to_iter b.instrs
  let instrs_backward_fold b = Vec.to_iter_rev b.instrs
end

module Graph = struct
  include T.Graph

  include Cfg.Graph.Make_gen (struct
      type 'r t = 'r Block.t

      let jumps_fold = Block.jumps_fold
    end)

  let map_blocks = Cfg.Graph.map
end

module Function = struct
  include T.Function

  let instrs_forward_fold fn = (Cfg.Graph.to_iter @> Block.instrs_forward_fold) fn.graph
  let map_blocks fn ~f = { fn with graph = Graph.map_blocks fn.graph ~f }
end

module Program = struct
  include T.Program

  let map_functions program ~f = { functions = List.map program.functions ~f }
end

module Dataflow = struct
  let instr_to_block_transfer ?(sexp_of_reg = sexp_of_opaque) trans =
    Cfg.Dataflow.instr_to_block_transfer
      ~sexp_of_block:(Block.sexp_of_t sexp_of_reg)
      ~instrs_forward_fold:Block.instrs_forward_fold
      ~instrs_backward_fold:Block.instrs_backward_fold
      trans
  ;;

  let run_block_transfer transfer (graph : _ Graph.t) =
    Cfg.Dataflow.run_block_transfer transfer
    @@ Cfg.Dataflow.Graph.of_cfg ~jumps:Block.jumps_fold graph
  ;;

  module Liveness = struct
    let instr_transfer =
      Cfg.Dataflow.Liveness.make_transfer
        ~sexp_of_instr:[%sexp_of: VReg.t Instr.t]
        ~value:(module VReg)
        ~uses:Instr.uses_fold
        ~defs:Instr.defs_fold
    ;;

    let block_transfer = instr_to_block_transfer instr_transfer
    let run fn = run_block_transfer block_transfer fn.Function.graph
  end
end
