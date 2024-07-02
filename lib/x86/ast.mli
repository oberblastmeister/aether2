open! O
module Name = Utils.Instr_types.Name
module Label = Utils.Instr_types.Label
module Stack_slot = Utils.Instr_types.Stack_slot
module Mach_reg_set : module type of Data.Enum_set.Make (Mach_reg)

module Reg_class : sig
  type t =
    | Int
    | Float
  [@@deriving compare, equal, sexp_of, variants]
end

module Size : sig
  type t =
    | Q
    | B
  [@@deriving equal, compare, hash, sexp]

  val to_byte_size : t -> int
end

module MReg : sig
  type t =
    { s : Size.t
    ; name : string option
    ; reg : Mach_reg.t
    }
  [@@deriving equal, compare, hash]

  val sexp_of_t : t -> Sexp.t
  val create : ?name:string -> Size.t -> Mach_reg.t -> t
end

module Stack_instr : sig
  type t =
    | ReserveEnd of { size : int32 }
    | ReserveLocal of
        { stack_slot : Stack_slot.t
        ; size : int32
        }
  [@@deriving sexp_of]
end

module Cmp_op : sig
  type t = Gt [@@deriving equal, compare, sexp]
end

module Stack_off : sig
  type t =
    (* to access arguments for this function *)
    | Start of int32
    (* use ReserveStackEnd *)
    (* used to put arguments on the stack to call a function *)
    | End of int32
    | Local of Stack_slot.t
  [@@deriving sexp_of]
end

module Imm_int : sig
  type t [@@deriving sexp_of, equal, compare]

  val of_z : Z.t -> t option
  val of_z_exn : Z.t -> t
  val to_z : t -> Z.t
  val of_int32 : int32 -> t
  val to_int64 : t -> int64
  val to_string : t -> string
  val to_string_hum : t -> string
  val to_encoded_uint32 : t -> Int_repr.Uint32.t
  val to_encoded_uint32_string : t -> string
end

module Abs_imm : sig
  type t [@@deriving sexp_of, equal, compare]

  val of_z : Z.t -> t option
  val of_z_exn : Z.t -> t
  val to_encoded_string : t -> string
end

module Imm : sig
  type t =
    | Int of Imm_int.t
    | Stack of Stack_off.t
  [@@deriving sexp_of, map, fold]
end

module Ty : sig
  type t =
    | U1
    | U64
  [@@deriving equal, compare, sexp, hash, variants]
end

module Mach_reg : sig
  include module type of Mach_reg

  val caller_saved_without_r11 : t list
  val caller_saved : t list
  val callee_saved_without_stack : t list
  val callee_saved : t list
  val args : t list
  val ret : t
end

module AReg : sig
  type t =
    | Spilled of
        { s : Size.t [@equal.ignore]
        ; name : Stack_slot.t
        }
    | InReg of
        { s : Size.t [@equal.ignore]
        ; name : string option [@equal.ignore]
        ; reg : Mach_reg.t
        }
  [@@deriving equal, sexp_of, variants]

  val reg_val : t -> Mach_reg.t option
  val size : t -> Size.t
  val create : ?name:string -> Size.t -> Mach_reg.t -> t
  val of_mreg : MReg.t -> t
end

module VReg : sig
  type t =
    { s : Size.t [@equal.ignore] [@compare.ignore] [@hash.ignore]
    ; name : Name.t
    }
  [@@deriving equal, compare, sexp, hash, fields]

  include Comparable.S with type t := t

  val create : Size.t -> Name.t -> t
end

module Address : sig
  module Base : sig
    type 'r t =
      | None
      | Reg of 'r
      | Rip
      | Rsp
    [@@deriving sexp_of, variants, map, fold, iter]

    val iter_regs : 'r t -> f:('r -> unit) -> unit
  end

  module Scale : sig
    type t =
      | One
      | Two
      | Four
      | Eight
    [@@deriving equal, compare, sexp, hash, sexp]

    val to_int : t -> int
  end

  module Index : sig
    type 'r t =
      | None
      | Some of
          { index : 'r
          ; scale : Scale.t
          }
    [@@deriving sexp_of, variants, map, fold, iter]

    val iter_regs : 'r t -> f:('r -> unit) -> unit
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

  val stack_offset : Stack_off.t -> 'a t
  val stack_off_end : int32 -> 'a t
  val stack_local : Stack_slot.t -> 'a t
  val base : 'a Base.t -> 'a t
  val index_scale : 'a -> Scale.t -> 'a t
  val base_offset : 'a Base.t -> Imm.t -> 'a t
  val rip_relative : Imm.t -> 'a t
  val iter_regs : 'r t -> f:('r -> unit) -> unit
end

module Mem : sig
  type 'r t =
    { size : Size.t
    ; addr : 'r Address.t
    }
  [@@deriving sexp_of, map, fold, iter]

  val iter_regs : 'a t -> f:('a -> unit) -> unit
  val map_addr : 'a t -> f:('a Address.t -> 'b Address.t) -> 'b t
end

module Operand : sig
  type 'r t =
    | Imm of Imm.t
    | Reg of 'r
    | Mem of 'r Mem.t
  [@@deriving sexp_of, variants, map, fold, iter]

  val mem : Size.t -> 'a Address.t -> 'a t
  val imm : Imm_int.t -> 'a t
  val stack_off_end : Size.t -> int32 -> 'a t
  val stack_local : Size.t -> Stack_slot.t -> 'a t
  val vreg : Size.t -> Name.t -> VReg.t t
  val iter_reg_val : 'a t -> f:('a -> unit) -> unit
  val iter_mem_val : 'a t -> f:('a Mem.t -> unit) -> unit
  val iter_mem_regs : 'a t -> f:('a -> unit) -> unit
  val iter_any_regs : 'a t -> f:('a -> unit) -> unit
  val of_areg : AReg.t -> MReg.t t
end

module Block_call : sig
  type 'r t =
    { label : Label.t
    ; args : 'r list
    }
  [@@deriving sexp_of, fold, map, iter]

  val iter_uses : 'r t -> f:('r -> unit) -> unit
  val map_regs : 'a t -> f:('a -> 'b) -> 'b t
end

module Jump : sig
  type 'r t =
    | Jump of 'r Block_call.t
    | CondJump of
        { cond : Cond.t
        ; j1 : 'r Block_call.t
        ; j2 : 'r Block_call.t
        }
    | Ret of 'r Operand.t option
  [@@deriving sexp_of, fold, map, iter]

  val map_regs : 'a t -> f:('a -> 'b) -> 'b t
  val iter_uses : 'a t -> f:('a -> unit) -> unit
  val iter_block_calls : 'a t -> f:('a Block_call.t -> unit) -> unit
  val map_block_calls : 'a t -> f:('a Block_call.t -> 'a Block_call.t) -> 'a t
  val map_regs : 'a t -> f:('a -> 'a) -> 'a t
end

module Precolored : sig
  type 'r t =
    | Precolored of
        { s : Size.t
        ; reg : Mach_reg.t
        }
    | Reg of 'r
  [@@deriving sexp_of, equal, compare, map]
end

module VInstr : sig
  (* TODO: allow operands in par mov source *)
  (* TODO: put stack operations in separate type *)
  type 'r t =
    (* for ssa *)
    | Block_args of 'r list
    (* | Par_mov of ('r Precolored.t * 'r Precolored.t) list *)
    | Par_mov of ('r * 'r) list
  [@@deriving sexp_of, variants, map]

  val map_regs : 'a t -> f:('a -> 'b) -> 'b t
  val iter_regs : 'a t -> f:('a -> unit) -> unit
  val iter_defs : 'a t -> f:('a -> unit) -> unit
  val iter_uses : 'a t -> f:('a -> unit) -> unit
end

module Instr : sig
  type 'r t =
    | NoOp
    | Mov of
        { dst : 'r Operand.t
        ; src : 'r Operand.t
        }
    | MovZx of
        { dst_size : Size.t
        ; src_size : Size.t
        ; dst : 'r Operand.t
        ; src : 'r Operand.t
        }
    | Lea of
        { dst : 'r
        ; src : 'r Address.t
        }
    | Add of
        { dst : 'r Operand.t
        ; src1 : 'r Operand.t
        ; src2 : 'r Operand.t
        }
    | Sub of
        { dst : 'r Operand.t
        ; src1 : 'r Operand.t
        ; src2 : 'r Operand.t
        }
    | Push of { src : 'r }
    | Pop of { dst : 'r }
    | MovAbs of
        { dst : 'r Operand.t
        ; imm : Z.t
        }
    | Cmp of
        { src1 : 'r Operand.t
        ; src2 : 'r Operand.t
        }
    | Test of
        { src1 : 'r Operand.t
        ; src2 : 'r Operand.t
        }
    | Set of
        { cond : Cond.t
        ; dst : 'r Operand.t
        }
    | Call of
        { name : string
        ; reg_args : (Mach_reg.t * 'r) list
        ; defines : Mach_reg.t list (* caller saved registers*)
        ; dst : ('r * Mach_reg.t) option
        }
    | Jump of 'r Jump.t
    | Virt of 'r VInstr.t
  [@@deriving sexp_of, map]

  val jump_val : 'a t -> 'a Jump.t option
  val virt_val : 'a t -> 'a VInstr.t option
  val map_regs : 'a t -> f:('a -> 'b) -> 'b t

  val iter_operands_with
    :  'a t
    -> on_def:('a Operand.t -> unit)
    -> on_use:('a Operand.t -> unit)
    -> unit

  val iter_operands : 'a t -> f:('a Operand.t -> unit) -> unit
  val iter_regs : 'a t -> f:('a -> unit) -> unit
  val map_regs : 'a t -> f:('a -> 'b) -> 'b t
  val iter_defs : 'a t -> f:('a -> unit) -> unit
  val iter_uses : 'a t -> f:('a -> unit) -> unit
  val mov_to_reg_from_stack : Size.t -> Mach_reg.t -> Stack_slot.t -> MReg.t t
  val mov_to_stack_from_reg : Size.t -> Stack_slot.t -> Mach_reg.t -> MReg.t t
  val mach_reg_defs : 'a t -> (Mach_reg.t -> unit) -> unit
end

module Some_instr : sig
  type t = T : 'r Instr.t -> t [@@deriving sexp_of]
end

module Block : sig
  type 'r t = { instrs : ('r Instr.t, read) Vec.t } [@@deriving sexp_of, fields]

  val map_regs : 'a t -> f:('a -> 'b) -> 'b t
  val map_instrs : 'a t -> f:('a Instr.t -> 'b Instr.t) -> 'b t
  val first_instr : 'a t -> 'a Instr.t option
  val jump_exn : 'a t -> 'a Jump.t
  val block_args_exn : 'a t -> 'a list
  val cons : 'a Instr.t -> 'a t -> 'a t
  val iter_jumps : 'a t -> f:(Label.t -> unit) -> unit
  val iter_instrs_forward : 'a t -> f:('a Instr.t -> unit) -> unit
  val iter_instrs_backward : 'a t -> f:('a Instr.t -> unit) -> unit
end

module Graph : sig
  type 'r t = 'r Block.t Cfg.Graph.t [@@deriving sexp_of]

  val map_regs : 'r t -> f:('r -> 's) -> 's t

  include Cfg.Graph.Gen_S with type 'r block := 'r Block.t
end

module Function : sig
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

  val map_regs : 'a t -> f:('a -> 'a) -> 'a t
  val all_params : 'a t -> 'a list
  val iter_instrs_forward : 'a t -> f:('a Instr.t -> unit) -> unit
  (* val map_blocks : 'a t -> f:('a Block.t -> 'b Block.t) -> 'b t *)
end

module Program : sig
  type 'r t = { functions : 'r Function.t list } [@@deriving sexp_of, fields]

  val map_functions : 'a t -> f:('a Function.t -> 'b Function.t) -> 'b t
  val iter_functions : 'a t -> f:('a Function.t -> unit) -> unit
end
