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

module MReg = struct
  include T.MReg
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
  let stack_local name = Imm (Imm.Stack (Stack_off.Local name))
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
end

module Block_call = struct
  include T.Block_call

  let args (type r) (instr : r t) =
    match instr.args with
    | Block_call args -> args
    | No_block_call -> []
  ;;

  let uses_fold (type r) (instr : r t) (k : r -> unit) =
    match instr.args with
    | Block_call args -> List.iter args ~f:k
    | No_block_call -> ()
  ;;

  let regs_fold block_call k = uses_fold block_call k
end

module Maybe_block_call = struct
  include T.Maybe_block_call
end

module Jump = struct
  include T.Jump

  let regs_fold i k =
    match i with
    | Jump j -> Block_call.regs_fold j k
    | CondJump { j1; j2; _ } ->
      Block_call.regs_fold j1 k;
      Block_call.regs_fold j2 k
  ;;

  let uses_fold i k =
    match i with
    | Jump j -> Block_call.uses_fold j k
    | CondJump { j1; j2; _ } ->
      Block_call.uses_fold j1 k;
      Block_call.uses_fold j2 k
  ;;

  let block_calls_fold j k =
    match j with
    | Jump j -> k j
    | CondJump { j1; j2; _ } ->
      k j1;
      k j2
  ;;

  let map_block_calls j ~f =
    match j with
    | Jump j -> Jump (f j)
    | CondJump { j1; j2; cond } -> CondJump { j1 = f j1; j2 = f j2; cond }
  ;;
end

module VInstr = struct
  include T.VInstr

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

module MInstr = struct
  include T.MInstr

  let operands_fold_with i ~on_def ~on_use =
    let module O = Operand in
    let module A = Address in
    match i with
    | NoOp -> ()
    | Lea { dst; src; _ } ->
      on_def @@ O.Reg dst;
      on_use @@ O.Mem src
    | Add { dst; src1; src2; _ } ->
      on_def dst;
      on_use src1;
      on_use src2
    | MovImm64 { dst; _ } -> on_def dst
    | Mov { dst; src; _ } ->
      on_def dst;
      on_use src
    | Cmp { src1; src2; _ } | Test { src1; src2; _ } ->
      on_use src1;
      on_use src2
    | Set { dst; _ } -> on_def dst
    | Ret -> ()
    | Push { src; _ } -> on_use @@ O.Reg src
    | Pop { dst; _ } -> on_def @@ O.Reg dst
  ;;

  let operands_fold i k = operands_fold_with i ~on_def:k ~on_use:k
  let regs_fold i k = operands_fold i (fun o -> Operand.any_regs_fold o k)

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
end

module Block = struct
  include T.Block

  let get_jump block =
    Vec.last block.instrs
    |> Option.value_or_thunk ~default:(fun () ->
      raise_s [%message "block must have a last instr"])
    |> Instr.to_variant
    |> fun instr ->
    Instr_variant.jump_val instr
    |> Option.value_or_thunk ~default:(fun () ->
      raise_s [%message "last instr must be a jump" (instr : _ Instr_variant.t)])
  ;;

  let get_block_args block =
    Vec.first block.instrs
    |> Option.value_or_thunk ~default:(fun () ->
      raise_s [%message "block must have a first instr"])
    |> Instr.to_variant
    |> fun instr ->
    (let open Option.Let_syntax in
     let%bind virt = Instr_variant.virt_val instr in
     let%bind block_args = VInstr.block_args_val virt in
     return block_args)
    |> Option.value_or_thunk ~default:(fun () -> raise_s [%message ""])
  ;;

  let replace_first instr ({ instrs } as block) =
    let _ = get_block_args block in
    let instrs = Vec.copy_exact instrs in
    Vec.set instrs 0 instr;
    let instrs = Vec.freeze instrs in
    { instrs }
  ;;

  (* |> Option.value ~default:(Instr.Virt (VInstr.Block_args []))
     |> function
     | Instr.Virt (VInstr.Block_args args) -> args
     | _ -> [] *)

  let jumps_fold block k =
    match get_jump block with
    | Jump j -> k j.label
    | CondJump { j1; j2; _ } ->
      k j1.label;
      k j2.label
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
end

module Function = struct
  include T.Function

  let instrs_forward_fold fn = (Cfg.Graph.to_iter @> Block.instrs_forward_fold) fn.graph
end

module Program = struct
  include T.Program
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
