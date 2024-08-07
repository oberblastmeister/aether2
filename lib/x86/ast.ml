open! O
module Name = Utils.Instr_types.Name
module Label = Utils.Instr_types.Label
module Stack_slot = Utils.Instr_types.Stack_slot
module Control = Utils.Instr_types.Control
module Mach_reg_set = Data.Enum_set.Make (Mach_reg)

module Size = struct
  type t =
    | Q
    | B
  [@@deriving equal, compare, hash, sexp]

  let to_byte_size = function
    | Q -> 8
    | B -> 1
  ;;
end

module Reg_class = struct
  type t = Int [@@deriving hash, compare, equal, sexp, variants]

  let of_mach_reg = function
    | Mach_reg.RAX
    | Mach_reg.RBX
    | Mach_reg.RCX
    | Mach_reg.RDX
    | Mach_reg.RSI
    | Mach_reg.RDI
    | Mach_reg.RBP
    | Mach_reg.RSP
    | Mach_reg.R8
    | Mach_reg.R9
    | Mach_reg.R10
    | Mach_reg.R11
    | Mach_reg.R12
    | Mach_reg.R13
    | Mach_reg.R14
    | Mach_reg.R15 -> Int
  ;;

  let scratch_reg_of_class = function
    | Int -> Mach_reg.R11
  ;;

  let max_size = function
    | Int -> Size.Q
  ;;
end

module MReg = struct
  type t =
    { name : string option [@equal.ignore] [@compare.ignore] [@hash.ignore]
    ; reg : Mach_reg.t
    }
  [@@deriving equal, compare, hash]

  let sexp_of_t { name; reg; _ } =
    match name with
    | None -> [%sexp (reg : Mach_reg.t)]
    | Some name -> [%sexp (name : string), (reg : Mach_reg.t)]
  ;;

  let create ?name reg = { name; reg }
end

module Stack_instr = struct
  type t =
    | ReserveEnd of { size : int32 }
    | ReserveLocal of
        { stack_slot : Stack_slot.t
        ; size : int32
        }
  [@@deriving sexp_of]
end

module Cmp_op = struct
  type t = Gt [@@deriving equal, compare, sexp]
end

module Stack_off = struct
  type t =
    (* to access arguments for this function *)
    | Start of int32
    (* use ReserveStackEnd *)
    (* used to put arguments on the stack to call a function *)
    | End of int32
    | Local of Stack_slot.t
  [@@deriving sexp_of]
end

let uint32_to_z u = Z.of_int64_unsigned (Int_repr.Uint32.to_base_int64 u)

module Imm_int = struct
  (* 32 bit signed or 32 bit unsigned *)
  (* represent the actual number instead of bit pattern *)
  (* thats why we use int64 *)
  type t = int64 [@@deriving sexp_of, equal, compare]

  let of_z z =
    let module Uint32 = Int_repr.Uint32 in
    if Z.(of_int32 Int32.min_value <= z && z <= of_int32 Int32.max_value)
    then Some (Z.to_int64 z)
    else if Z.(z <= of_int 0 && z <= uint32_to_z Uint32.max_value)
    then Some (Z.to_int64 z)
    else None
  ;;

  let of_z_exn z = Option.value_exn (of_z z)
  let of_int32 = Int64.of_int32
  let to_int64 = Fn.id
  let to_z = Z.of_int64

  let to_encoded_uint32 i =
    let module Uint32 = Int_repr.Uint32 in
    if Int64.(i < 0L)
    then
      Uint32.of_base_int64_exn Int64.(Int64.shift_left 1L 31 + Int64.shift_left 1L 31 + i)
    else Uint32.of_base_int64_exn i
  ;;

  (* make sure this isn't using Int64.to_string_hum because that one uses underscores*)
  let to_encoded_uint32_string i =
    Int64.to_string (Int_repr.Uint32.to_base_int64 (to_encoded_uint32 i))
  ;;

  let to_string = Int64.to_string
  let to_string_hum i = Int64.to_string_hum i
end

module Abs_imm = struct
  type t = Z.t [@@deriving sexp_of, equal, compare]

  let of_z z = if Z.(Z.of_int64 Int64.min_value <= z && true) then Some z else None
  let of_z_exn z = Option.value_exn (of_z z)
  let to_encoded_string _ = todo [%here]
end

module Imm = struct
  type t =
    | Int of Imm_int.t
    | Label of string
    | LabelGot of string
    | LabelPlt of string
    | Stack of Stack_off.t
  [@@deriving sexp_of, map, fold]
end

module Ty = struct
  type t =
    | I1
    | U64
  [@@deriving equal, compare, sexp, hash, variants]
end

module Mach_reg = struct
  include Mach_reg

  let caller_saved_without_r11 = [ RAX; RDI; RSI; RDX; RCX; R8; R9; R10 ]
  let caller_saved = caller_saved_without_r11 @ [ R11 ]
  let callee_saved_without_stack = [ RBX; R12; R13; R14; R15 ]
  let callee_saved = [ RSP; RBP ] @ callee_saved_without_stack
  let args = [ RDI; RSI; RDX; RCX; R8; R9 ]
  let ret = RAX
end

module AReg = struct
  type t =
    | Spilled of
        { reg_class : Reg_class.t [@equal.ignore]
        ; name : Stack_slot.t
        }
    | InReg of MReg.t
  [@@deriving equal, sexp_of, variants]

  let reg_class = function
    | Spilled { reg_class; _ } -> reg_class
    | InReg { reg; _ } -> Reg_class.of_mach_reg reg
  ;;

  let reg_val = function
    | InReg { reg; _ } -> Some reg
    | _ -> None
  ;;

  let create ?name reg = InReg { name; reg }
  let of_mreg (mreg : MReg.t) = InReg mreg
end

module VReg = struct
  (* make this a variant so we can have unnamed virtual registers *)
  module T = struct
    type t =
      { reg_class : Reg_class.t [@equal.ignore] [@compare.ignore] [@hash.ignore]
      ; name : Name.t
      }
    [@@deriving equal, compare, sexp, hash, fields]
  end

  include T
  include Comparable.Make (T)

  (* let to_name r = r.name *)
  let create ?(reg_class = Reg_class.Int) name = { reg_class; name }
end

(* currently nested registers in addressing modes are assumed to by of size Q *)
module Address = struct
  module Base = struct
    type 'r t =
      | None
      | Reg of 'r
      | Rip
      | Rsp
    [@@deriving sexp_of, variants, map, fold, iter]

    let iter_regs a ~f = reg_val a |> Option.iter ~f
  end

  module Scale = struct
    type t =
      | One
      | Two
      | Four
      | Eight
    [@@deriving equal, compare, sexp, hash, sexp]

    let to_int = function
      | One -> 1
      | Two -> 2
      | Four -> 4
      | Eight -> 8
    ;;
  end

  module Index = struct
    type 'r t =
      | None
      | Some of
          { index : 'r
          ; scale : Scale.t
          }
    [@@deriving sexp_of, variants, map, fold, iter]

    let iter_regs a ~f =
      some_val a |> Option.iter ~f:(fun (`index index, `scale _) -> f index)
    ;;
  end

  type 'r t =
    | Imm of
        { offset : Imm.t
        ; scale : Scale.t
        }
    | Complex of
        { base : 'r Base.t
        ; index : 'r Index.t
        ; offset : Imm.t
        }
  [@@deriving sexp_of, map, fold, iter]

  let stack_offset imm = Complex { base = Rsp; index = None; offset = Stack imm }
  let stack_off_end i = stack_offset (End i)
  let stack_local name = stack_offset (Local name)
  let base base = Complex { base; index = None; offset = Int 0L }
  let reg r = base (Reg r)

  let index_scale index scale =
    Complex { base = None; index = Some { index; scale }; offset = Int 0L }
  ;;

  let base_offset base offset = Complex { base; index = None; offset }
  let rip_relative offset = base_offset Base.Rip offset

  let iter_regs a ~f =
    match a with
    | Imm _ -> ()
    | Complex { base; index; _ } ->
      Base.iter_regs base ~f;
      Index.iter_regs index ~f
  ;;
end

module Simple_operand = struct
  type 'r t =
    | Imm of Imm.t
    | Reg of 'r
  [@@deriving sexp_of, variants, map, fold, iter]

  let iter_regs o ~f:k =
    match o with
    | Reg r -> k r
    | Imm _ -> ()
  ;;
end

module Operand = struct
  type 'r t =
    | Imm of Imm.t
    | Reg of 'r
    | Mem of 'r Address.t
  [@@deriving sexp_of, variants, map, fold, iter]

  let of_simple = function
    | Simple_operand.Imm i -> Imm i
    | Reg r -> Reg r
  ;;

  let imm i = Imm (Int i)
  let stack_off_end i = mem (Address.stack_off_end i)
  let stack_local name = mem (Address.stack_local name)
  let vreg name = Reg (VReg.create name)
  let iter_reg_val o ~f:k = reg_val o |> Option.iter ~f:k
  let iter_mem_val o ~f:k = mem_val o |> Option.iter ~f:k
  let iter_mem_regs o ~f:k = (iter_mem_val @> Address.iter_regs) o ~f:k

  let iter_any_regs o ~f:k =
    match o with
    | Reg r -> k r
    | Mem m -> Address.iter_regs m ~f:k
    | Imm _ -> ()
  ;;
end

module Block_call = struct
  type 'r t =
    { label : Label.t
    ; args : 'r Simple_operand.t list
    }
  [@@deriving sexp_of, fold, map, iter]

  let iter_uses instr ~f = (List.iter @> Simple_operand.iter_regs) instr.args ~f
  let map_regs i ~f = map f i
end

module Jump = struct
  type 'r t =
    | Jump of 'r Block_call.t
    | CondJump of
        { cond : Cond.t
        ; j1 : 'r Block_call.t
        ; j2 : 'r Block_call.t
        }
    | Ret of (Size.t * 'r Operand.t) option
  [@@deriving sexp_of, fold, map, iter]

  let map_regs i ~f = map f i

  let iter_uses i ~f =
    match i with
    | Jump j -> Block_call.iter_uses j ~f
    | CondJump { j1; j2; _ } ->
      Block_call.iter_uses j1 ~f;
      Block_call.iter_uses j2 ~f
    | Ret r -> Option.iter r ~f:(fun (_s, o) -> Operand.iter_any_regs o ~f)
  ;;

  let iter_block_calls j ~f:k =
    match j with
    | Jump j -> k j
    | CondJump { j1; j2; _ } ->
      k j1;
      k j2
    | Ret _ -> ()
  ;;

  let map_block_calls j ~f =
    match j with
    | Jump j -> Jump (f j)
    | CondJump { j1; j2; cond } -> CondJump { j1 = f j1; j2 = f j2; cond }
    | Ret r -> Ret r
  ;;

  let map_regs i = (map_block_calls & Block_call.map_regs) i
end

module Precolored = struct
  type 'r t =
    | Precolored of
        { s : Size.t
        ; reg : Mach_reg.t
        }
    | Reg of 'r
  [@@deriving sexp_of, equal, compare, map]
end

module VInstr = struct
  type 'r t =
    (* for ssa *)
    | Block_args of 'r list
    (* should only be between registers of the same class
       because registers of different classes can't effect each other anyways
    *)
    | Par_mov of ('r * 'r Simple_operand.t) list
  [@@deriving sexp_of, variants, map]

  let map_regs i ~f = map f i

  let iter_regs i ~f =
    match i with
    | Par_mov movs ->
      (List.iter
       @> fun (x, y) ~f ->
       f x;
       Simple_operand.iter_regs ~f y)
        movs
        ~f
    | Block_args regs -> List.iter regs ~f
  ;;

  let iter_defs i ~f =
    match i with
    | Par_mov movs -> (List.iter @> F.Fold.of_fn fst) movs ~f
    | Block_args args -> List.iter args ~f
  ;;

  let iter_uses i ~f =
    match i with
    | Par_mov movs -> (List.iter @> F.Fold.of_fn snd @> Simple_operand.iter_regs) movs ~f
    | Block_args _ -> ()
  ;;
end

module Instr = struct
  type 'r t =
    | NoOp
    | Mov of
        { s : Size.t
        ; dst : 'r Operand.t
        ; src : 'r Operand.t
        }
    | MovZx of
        { dst_size : Size.t
        ; src_size : Size.t
        ; dst : 'r Operand.t
        ; src : 'r Operand.t
        }
    | Lea of
        { s : Size.t
        ; dst : 'r
        ; src : 'r Address.t
        }
    | Add of
        { s : Size.t
        ; dst : 'r Operand.t
        ; src1 : 'r Operand.t
        ; src2 : 'r Operand.t
        }
    | Sub of
        { s : Size.t
        ; dst : 'r Operand.t
        ; src1 : 'r Operand.t
        ; src2 : 'r Operand.t
        }
    | Imul of
        { s : Size.t
        ; dst : 'r Operand.t
        ; src1 : 'r Operand.t
        ; src2 : 'r Operand.t
        }
    | Div of
        { s : Size.t
        ; dst : 'r Operand.t (* the quotient *)
        ; src1 : 'r Operand.t (* the dividend *)
        ; src2 : 'r Operand.t (* the divisor *)
        }
    | Idiv of
        { s : Size.t
        ; dst : 'r Operand.t
        ; src1 : 'r Operand.t
        ; src2 : 'r Operand.t
        }
    | Push of
        { s : Size.t
        ; src : 'r
        }
    | Pop of
        { s : Size.t
        ; dst : 'r
        }
    | MovAbs of
        { (* dst size is always Q *)
          dst : 'r Operand.t
        ; imm : Z.t
        }
    | Cmp of
        { s : Size.t
        ; src1 : 'r Operand.t
        ; src2 : 'r Operand.t
        }
    | Test of
        { s : Size.t
        ; src1 : 'r Operand.t
        ; src2 : 'r Operand.t
        }
    | Set of
        { (* dst size is always B *)
          cond : Cond.t
        ; dst : 'r Operand.t
        }
    | Call of
        { (* dst size is always Q *)
          name : string
        ; use_plt : bool
        ; reg_args : (Mach_reg.t * 'r Simple_operand.t) list
        ; defines : Mach_reg.t list (* caller saved registers*)
        ; dst : ('r * Mach_reg.t) option
        }
    | Jump of 'r Jump.t
    | Virt of 'r VInstr.t
  [@@deriving sexp_of, map]

  let sexp_of_t f = function
    | Virt v -> VInstr.sexp_of_t f v
    | Jump v -> Jump.sexp_of_t f v
    | instr -> sexp_of_t f instr
  ;;

  let jump_val = function
    | Jump j -> Some j
    | _ -> None
  ;;

  let virt_val = function
    | Virt v -> Some v
    | _ -> None
  ;;

  let map_regs t ~f = map f t

  (* add a on_mach_reg parameter *)
  (* then add a specialized fold_mach_regs *)
  let iter_operands_with i ~on_def ~on_use =
    let module O = Operand in
    let module A = Address in
    match i with
    | NoOp -> ()
    | Lea { dst; src; _ } ->
      on_def @@ O.Reg dst;
      Address.iter_regs src ~f:(fun reg -> on_use (O.Reg reg))
    | Sub { dst; src1; src2; _ }
    | Add { dst; src1; src2; _ }
    | Imul { dst; src1; src2; _ }
    | Div { dst; src1; src2; _ }
    | Idiv { dst; src1; src2; _ } ->
      on_def dst;
      on_use src1;
      on_use src2
    | MovAbs { dst; _ } -> on_def dst
    | Mov { dst; src; _ } ->
      on_def dst;
      on_use src
    | MovZx { dst; src; _ } ->
      on_def dst;
      on_use src
    | Cmp { src1; src2; _ } | Test { src1; src2; _ } ->
      on_use src1;
      on_use src2
    | Set { dst; _ } -> on_def dst
    | Push { src; _ } -> on_use @@ O.Reg src
    | Pop { dst; _ } -> on_def @@ O.Reg dst
    | Call { reg_args; dst; _ } ->
      (List.iter @> F.Fold.of_fn snd @> Simple_operand.iter_regs) reg_args ~f:(fun arg ->
        on_use (Reg arg));
      Option.iter dst ~f:(fun (dst, _) -> on_def (Reg dst));
      ()
    | Jump jump -> Jump.iter_uses jump ~f:(fun reg -> on_use (Reg reg))
    | Virt vinstr ->
      VInstr.iter_uses vinstr ~f:(fun reg -> on_use (Reg reg));
      VInstr.iter_defs vinstr ~f:(fun reg -> on_def (Reg reg));
      ()
  ;;

  let iter_operands i ~f = iter_operands_with i ~on_def:f ~on_use:f
  let iter_regs i ~f = iter_operands i ~f:(fun o -> Operand.iter_any_regs o ~f)
  let map_regs i ~f = map f i

  let iter_defs i ~f =
    iter_operands_with
      i
      ~on_def:(fun o -> Operand.iter_reg_val o ~f)
      ~on_use:(Fn.const ())
  ;;

  let iter_uses i ~f =
    iter_operands_with
      i
      ~on_def:(fun o -> Operand.iter_mem_regs o ~f)
      ~on_use:(fun o -> Operand.iter_any_regs o ~f)
  ;;

  let mov_to_reg_from_stack s reg (stack_name : Stack_slot.t) =
    Mov
      { s
      ; dst = Reg (MReg.create ~name:stack_name.name reg)
      ; src = Operand.mem (Address.stack_local stack_name)
      }
  ;;

  let mov_to_stack_from_reg s (stack_name : Stack_slot.t) reg =
    Mov
      { s
      ; dst = Operand.mem (Address.stack_local stack_name)
      ; src = Reg (MReg.create ~name:stack_name.name reg)
      }
  ;;

  let mach_reg_defs i k =
    match i with
    | Call { defines; _ } -> List.iter defines ~f:k
    | Div _ | Idiv _ ->
      k Mach_reg.RAX;
      k Mach_reg.RDX;
      k Mach_reg.RDX;
      ()
    | Imul _ (* the two operand version of imul uses no machine registers *)
    | NoOp
    | Mov _
    | MovZx _
    | Lea _
    | Add _
    | Sub _
    | Push _
    | Pop _
    | MovAbs _
    | Cmp _
    | Test _
    | Set _
    | Jump _
    | Virt _ -> ()
  ;;
end

module Some_instr = struct
  type t = T : 'r Instr.t -> t [@@deriving sexp_of]
end

module Block = struct
  type 'r t = { instrs : ('r Instr.t, read) Vec.t } [@@deriving sexp_of, fields]

  let map_regs b ~f = { b with instrs = (Vec.map_copy & Instr.map_regs) b.instrs ~f }
  let map_instrs b ~f = { b with instrs = Vec.map_copy b.instrs ~f }
  let first_instr b = Vec.first b.instrs

  let jump_exn block =
    Vec.last block.instrs
    |> Option.value_or_thunk ~default:(fun () ->
      raise_s [%message "block must have a last instr"])
    |> fun instr ->
    Instr.jump_val instr
    |> Option.value_or_thunk ~default:(fun () ->
      raise_s [%message "last instr must be a jump" (instr : _ Instr.t)])
  ;;

  let block_args_exn block =
    Vec.first block.instrs
    |> Option.value_or_thunk ~default:(fun () ->
      raise_s [%message "block must have a first instr"])
    |> fun instr ->
    instr
    |> fun instr ->
    (let open Option.Let_syntax in
     let%bind virt = Instr.virt_val instr in
     let%bind block_args = VInstr.block_args_val virt in
     return block_args)
    |> Option.value_or_thunk ~default:(fun () ->
      raise_s [%message "first instruction must be block arguments" (instr : _ Instr.t)])
  ;;

  let cons instr b = { instrs = Vec.cons instr b.instrs }

  let iter_jumps block ~f =
    match jump_exn block with
    | Jump j -> f j.label
    | CondJump { j1; j2; _ } ->
      f j1.label;
      f j2.label
    | Ret _ -> ()
  ;;

  let iter_instrs_forward b = Vec.iter b.instrs
  let iter_instrs_backward b = Vec.iter_rev b.instrs
end

module Graph = struct
  type 'r t = 'r Block.t Cfg.Graph.t [@@deriving sexp_of]

  let map_regs g ~f = (Cfg.Graph.map & Block.map_regs) ~f g

  include Cfg.Graph.Make_gen (struct
      type 'r t = 'r Block.t

      let iter_jumps = Block.iter_jumps
    end)
end

module Function = struct
  type 'r t =
    { name : string
    ; graph : 'r Graph.t
    ; params : ('r * MReg.t) list
    ; stack_params : 'r list
    ; unique_name : Name.Id.t
    ; unique_stack_slot : Stack_slot.Id.t
    ; caller_saved : Mach_reg.t list
    ; stack_instrs : Stack_instr.t list
    }
  [@@deriving sexp_of, fields]

  let map_regs fn ~f = { fn with graph = Graph.map_regs ~f fn.graph }
  let all_params fn = List.map fn.params ~f:fst @ fn.stack_params
  let iter_instrs_forward fn = (Cfg.Graph.iter @> Block.iter_instrs_forward) fn.graph
  let map_blocks fn ~f = { fn with graph = Cfg.Graph.map fn.graph ~f }
end

module Section = struct
  type t =
    | Text
    | Data
    | Rodata
  [@@deriving sexp_of, equal, compare]

  let to_string = function
    | Text -> ".text"
    | Data -> ".data"
    | Rodata -> ".rodata"
  ;;
end

module Data_item = struct
  type t =
    | String of string
    | Bytes of string
  [@@deriving sexp_of]
end

module Data_decl = struct
  type t =
    { name : string
    ; section : Section.t
    ; align : int
    ; data : Data_item.t
    }
  [@@deriving sexp_of]
end

module Program = struct
  type 'r t =
    { functions : 'r Function.t list
    ; data_decls : Data_decl.t list
    }
  [@@deriving sexp_of, fields]

  let of_function f = { functions = [ f ]; data_decls = [] }
  let of_data_decl d = { functions = []; data_decls = [ d ] }

  let map_functions program ~f =
    { functions = List.map program.functions ~f; data_decls = program.data_decls }
  ;;

  let iter_functions program ~f = List.iter program.functions ~f
  let empty_program = { functions = []; data_decls = [] }

  let combine_program program1 program2 =
    { functions = program1.functions @ program2.functions
    ; data_decls = program1.data_decls @ program2.data_decls
    }
  ;;
end
