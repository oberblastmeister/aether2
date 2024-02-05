(* TODO: remove generic types and stuff *)
open! O
open Utils.Instr_types

module Size = struct
  type t =
    | Q
    | L
    | W
    | B
  [@@deriving equal, compare, hash, sexp]
end

module Cond = struct
  type t =
    (* equal*)
    | E
    (* not equal*)
    | NE
    (* below (unsigned <) *)
    | B
    (* below or equal *)
    | BE
    (* above (unsigned >) *)
    | A
  [@@deriving equal, compare, hash, sexp, variants]
end

module Ty = struct
  type t =
    | U1
    | U64
  [@@deriving equal, compare, sexp, hash, variants]
end

module Scale = struct
  type t =
    | Scale1
    | Scale2
    | Scale4
    | Scale8
  [@@deriving equal, compare, sexp, hash, sexp]

  let to_int = function
    | Scale1 -> 1
    | Scale2 -> 2
    | Scale4 -> 4
    | Scale8 -> 8
  ;;
end

module Mach_reg = struct
  type t =
    | RAX
    | RCX
    | RDX
    | RBX
    | RSP
    | RBP
    | RSI
    | RDI
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
  [@@deriving equal, compare, sexp, hash, variants]
end

module MReg = struct
  type t =
    { s : Size.t
    ; reg : Mach_reg.t
    }
  [@@deriving equal, compare, sexp, hash]
end

module VReg = struct
  module T = struct
    type t =
      { s : Size.t [@equal.ignore] [@compare.ignore] [@hash.ignore]
      ; name : Name.t
      ; precolored : Mach_reg.t option [@equal.ignore] [@compare.ignore] [@hash.ignore]
      }
    [@@deriving equal, compare, sexp, hash]
  end

  include T
  include Comparator.Make (T)

  let to_name r = r.name
  let create s name = { s; name; precolored = None }
  let precolored s name precolored = { s; name; precolored = Some precolored }
end

module Address = struct
  type 'r t =
    { base : 'r
    ; index : 'r
    ; scale : Scale.t
    ; displacement : int32
    }
  [@@deriving equal, compare, sexp, hash, fields]
end

module Stack_off = struct
  type t =
    | End of int32
    | Any
  [@@deriving sexp_of]
end

module Imm = struct
  type 'r t =
    | Int : int32 -> 'r t
    | Stack : Stack_off.t -> VReg.t t
  [@@deriving sexp_of]

  let get (Int i) = i
end

module Operand = struct
  type 'r t =
    | Imm of 'r Imm.t
    | Reg of 'r
    | Mem of 'r Address.t
  [@@deriving sexp_of, variants]

  let imm i = Imm (Imm.Int i)
  let stack_off i = Imm (Imm.Stack (Stack_off.End i))
  let stack_loc = Imm (Imm.Stack Stack_off.Any)
end

module Cmp_op = struct
  type t = Gt [@@deriving equal, compare, sexp]
end

module MInstr = struct
  type 'r t =
    | Mov of
        { s : Size.t
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
    | Push of { src : 'r }
    | Pop of { dst : 'r }
    | MovImm64 of
        { dst : 'r Operand.t
        ; imm : int64
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
        { s : Size.t
        ; cond : Cond.t
        ; dst : 'r Operand.t
        }
    | Ret
  [@@deriving sexp_of]
end

module VInstr = struct
  type t =
    | Def of { dst : VReg.t }
    | Block_args of VReg.t list
    | ReserveStackEnd of int32
    | Par_mov of (VReg.t * VReg.t) list
  [@@deriving sexp_of]
end

module Maybe_block_call = struct
  type 'r t =
    | Block_call : VReg.t list -> VReg.t t
    | No_block_call : Mach_reg.t t
  [@@deriving sexp_of]
end

module Block_call = struct
  type 'r t =
    { label : Label.t
    ; args : 'r Maybe_block_call.t
    }
  [@@deriving sexp_of]
end

module Jump = struct
  type 'r t =
    | Jump of 'r Block_call.t
    | CondJump of
        { cond : Cond.t
        ; j1 : 'r Block_call.t
        ; j2 : 'r Block_call.t
        }
  [@@deriving sexp_of]
end

module Instr_variant = struct
  type 'r t =
    | Virtual of VInstr.t
    | Real of 'r MInstr.t
    | Jump of 'r Jump.t
  [@@deriving sexp_of]
end

module Instr = struct
  type 'r t =
    | Virtual : VInstr.t -> VReg.t t
    | Real : 'r MInstr.t -> 'r t
    | Jump : 'r Jump.t -> 'r t
  [@@deriving sexp_of]
end

module Some_instr = struct
  type t = T : 'r Instr.t -> t [@@deriving sexp_of]
end

module Block = struct
  type 'r t = { instrs : ('r Instr.t, Perms.Read.t) Vec.t } [@@deriving sexp_of, fields]
end

module Graph = struct
  type 'r t = 'r Block.t Cfg.Graph.t [@@deriving sexp_of]
end

module Function = struct
  type 'r t = { graph : 'r Graph.t } [@@deriving sexp_of, fields]
end

module Program = struct
  type 'r t = { functions : 'r Function.t list } [@@deriving sexp_of, fields]
end
