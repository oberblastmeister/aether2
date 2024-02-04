(* TODO: remove generic types and stuff *)
open! O
open Utils.Instr_types

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

module AllocReg = struct
  type t =
    | Reg of MachReg.t
    | Stack
  [@@deriving sexp]
end

module VReg = struct
  type t =
    | Temp of { name : Name.t }
    | PreColored of
        { name : Name.t
        ; reg : MachReg.t [@equal.ignore] [@compare.ignore] [@hash.ignore]
        }
  [@@deriving equal, compare, sexp, hash, variants]

  let to_name (Temp { name } | PreColored { name; _ }) = name
end

module Size = struct
  type t =
    | Q
    | L
    | W
    | B
  [@@deriving equal, compare, hash, sexp]
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
  type t =
    { base : Reg.t
    ; index : Reg.t
    ; scale : Scale.t
    ; displacement : int32
    }
  [@@deriving equal, compare, sexp, hash, fields]
end

module Operand = struct
  (* todo: add virtual stack offset thingy here *)
  (* so we don't need separate stack instructions *)
  type t =
    | Imm of int32
    | Reg of Reg.t
    | Mem of Address.t
  [@@deriving sexp, variants]
end

module Cmp_op = struct
  type t = Gt [@@deriving equal, compare, sexp]
end

module Block_call = struct
  type t =
    { label : Label.t
    ; args : Reg.t list
    }
  [@@deriving sexp, fields]
end

module Instr = struct
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
    | StoreStack of
        { s : Size.t
        ; dst : int32
        ; src : Operand.t
        }
    | LoadStack of
        { s : Size.t
        ; dst : Operand.t
        ; src : int32
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
end

module Block = struct
  type t = { instrs : (Instr.t, Perms.Read.t) Vec.t } [@@deriving sexp, fields]
end

module Graph = struct
  type t = Block.t Cfg.Graph.t [@@deriving sexp]
end

module Function = struct
  type t = { graph : Graph.t } [@@deriving sexp, fields]
end

module Program = struct
  type t = { functions : Function.t list } [@@deriving sexp, fields]
end
