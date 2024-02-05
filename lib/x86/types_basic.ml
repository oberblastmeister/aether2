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

module MachReg = struct
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
    ; reg : MachReg.t
    }
  [@@deriving equal, compare, sexp, hash]
end

module AllocReg = struct
  type t =
    | Reg of MachReg.t
    | Stack
  [@@deriving sexp]
end

module VReg = struct
  module T = struct
    type t =
      { s : Size.t [@equal.ignore] [@compare.ignore] [@hash.ignore]
      ; name : Name.t
      ; precolored : MachReg.t option [@equal.ignore] [@compare.ignore] [@hash.ignore]
      }
    [@@deriving equal, compare, sexp, hash]
  end

  include T
  include Comparator.Make (T)

  let to_name r = r.name
  let create s name = { s; name; precolored = None }
  let precolored s name precolored = { s; name; precolored = Some precolored }
end

module Reg_kind = struct
  type t =
    | VReg of VReg.t
    | MachReg of MachReg.t
  [@@deriving equal, compare, hash, sexp, variants]
end

module Reg = struct
  module T = struct
    type t =
      { s : Size.t [@equal.ignore] [@compare.ignore] [@hash.ignore]
      ; reg : Reg_kind.t
      }
    [@@deriving equal, compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)
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

module Imm = struct
  type 'r t =
    | Int : int32 -> 'r t
    | StackOff : int32 -> VReg.t t
  [@@deriving sexp_of]
end

module Operand = struct
  (* todo: add virtual stack offset thingy here *)
  (* so we don't need separate stack instructions *)
  (* type t =
     | Imm of int32
     | Reg of Reg.t
     | Mem of Address.t
     [@@deriving sexp, variants] *)

  type 'r t =
    | Imm of 'r Imm.t
    | Reg of 'r
    | Mem of 'r Address.t
  [@@deriving sexp_of, variants]

  let imm i = Imm (Imm.Int i)
  let stack_off i = Imm (Imm.StackOff i)
end

module Cmp_op = struct
  type t = Gt [@@deriving equal, compare, sexp]
end

(* module Block_call = struct
  type t =
    { label : Label.t
    ; args : VReg.t list
    }
  [@@deriving sexp, fields]
end *)

module Stack_off = struct
  type t =
    | Start of int32
    | End of int32
  [@@deriving sexp]
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
    | StoreStack of
        { s : Size.t
        ; dst : Stack_off.t
        ; src : VReg.t Operand.t
        }
    | LoadStack of
        { s : Size.t
        ; dst : VReg.t Operand.t
        ; src : Stack_off.t
        }
    | Par_mov of (VReg.t * VReg.t) list
  [@@deriving sexp_of]
end

module Maybe_block_call = struct
  type 'r t =
    | Block_call : VReg.t list -> VReg.t t
    | No_block_call : MachReg.t t
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

  (* let to_some (type r) instr =
     match instr with
     | Virtual v -> Some_instr.Virtual v
     | Real r -> Some_instr.Real r
     | Jump j -> Some_instr.Jump j
     ;; *)
end

module Some_instr = struct
  type t = T : 'r Instr.t -> t [@@deriving sexp_of]
end

(* module Instr = struct
  type t =
    | Lea of
        { s : Size.t
        ; dst : Reg.t
        ; src : Address.t
        }
    | Add of
        { s : Size.t
        ; dst : Operand.t
        ; src1 : Operand.t
        ; src2 : Operand.t
        }
    | Push of { src : Reg.t }
    | Pop of { src : Reg.t }
    | StoreStack of
        { s : Size.t
        ; dst : Stack_off.t
        ; src : Operand.t
        }
    | LoadStack of
        { s : Size.t
        ; dst : Operand.t
        ; src : Stack_off.t
        }
    | MovImm64 of
        { dst : Operand.t
        ; imm : int64
        }
    | Mov of
        { s : Size.t
        ; dst : Operand.t
        ; src : Operand.t
        }
    | Par_mov of (Reg.t * Reg.t) list
    | Cmp of
        { s : Size.t
        ; src1 : Operand.t
        ; src2 : Operand.t
        }
    | Test of
        { s : Size.t
        ; src1 : Operand.t
        ; src2 : Operand.t
        }
    | Set of
        { s : Size.t
        ; cond : Cond.t
        ; dst : Operand.t
        }
    (* for calling conventions *)
    | Def of { dst : Reg.t }
    | Block_args of Reg.t list
    | Jump of Block_call.t
    | CondJump of
        { cond : Cond.t
        ; j1 : Block_call.t
        ; j2 : Block_call.t
        }
    | Ret
  [@@deriving sexp, variants]

  let jumps_fold instr k =
    match instr with
    | Jump block_call -> k block_call.label
    | CondJump { j1; j2; _ } ->
      k j1.label;
      k j2.label
    | _ -> ()
  ;;
end *)

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
