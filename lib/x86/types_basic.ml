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
  [@@deriving enum, equal, compare, sexp, hash, variants]
end

module MReg = struct
  type t =
    { s : Size.t
    ; name : string option
    ; reg : Mach_reg.t
    }
  [@@deriving equal, compare, sexp, hash]

  let create ?name s reg = { s; name; reg }
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
  include Base.Comparable.Make (T)
end

module AReg = struct
  type t =
    | Spilled of
        { s : Size.t [@equal.ignore]
        ; name : Name.t
        }
    | InReg of
        { s : Size.t [@equal.ignore]
        ; name : string [@equal.ignore]
        ; reg : Mach_reg.t
        }
  [@@deriving equal, sexp_of, variants]
end

module Stack_off = struct
  type t =
    (* to access arguments for this function *)
    | Start of int32
    (* use ReserveStackEnd *)
    (* used to put arguments on the stack to call a function *)
    | End of int32
    | Local of Name.t
  [@@deriving sexp_of]
end

module Imm = struct
  type t =
    | Int of int32
    | Stack of Stack_off.t
  [@@deriving sexp_of, map, fold]
end

module Address = struct
  module Base = struct
    type 'r t =
      | None
      | Reg of 'r
      | Rip
    [@@deriving sexp_of, variants, map, fold]
  end

  module Scale = struct
    type t =
      | One
      | Two
      | Four
      | Eight
    [@@deriving sexp_of, variants]
  end

  module Index = struct
    type 'r t =
      | None
      | Some of
          { index : 'r
          ; scale : Scale.t
          }
    [@@deriving sexp_of, variants, map, fold]
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
  [@@deriving sexp_of, map, fold]
end

module Operand = struct
  type 'r t =
    | Imm of Imm.t
    | Reg of 'r
    | Mem of 'r Address.t
  [@@deriving sexp_of, variants, map, fold]
end

module Cmp_op = struct
  type t = Gt [@@deriving equal, compare, sexp]
end

module MInstr = struct
  type 'r t =
    | NoOp
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
    | MovAbs of
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
  [@@deriving sexp_of, map, fold]
end

module VInstr = struct
  type 'r t =
    (* for calling conventions*)
    | Def of { dst : 'r }
    | ReserveStackEnd of { size : int32 }
    | ReserveStackLocal of
        { name : Name.t
        ; size : int32
        }
    (* for ssa *)
    | Block_args of 'r list
    | Par_mov of ('r * 'r) list
  [@@deriving sexp_of, variants, map, fold]
end

(* module Maybe_block_call = struct
   type 'r t =
   | Block_call : VReg.t list -> VReg.t t
   | No_block_call : Mach_reg.t t
   [@@deriving sexp_of]

   let get_args b =
   match b with
   | Block_call args -> args
   | _ -> .
   ;;
   end *)

module Block_call = struct
  type 'r t =
    { label : Label.t
    ; args : 'r list
    }
  [@@deriving sexp_of, fold, map, iter]
end

module Jump = struct
  type 'r t =
    | Jump of 'r Block_call.t
    | CondJump of
        { cond : Cond.t
        ; j1 : 'r Block_call.t
        ; j2 : 'r Block_call.t
        }
    | Ret
  [@@deriving sexp_of, fold, map, iter]
end

module Instr_variant = struct
  type 'r t =
    | Virt of 'r VInstr.t
    | Real of 'r MInstr.t
    | Jump of 'r Jump.t
  [@@deriving sexp_of, variants]
end

module Instr = struct
  type 'r t =
    | Virt : 'r VInstr.t -> 'r t
    | Real : 'r MInstr.t -> 'r t
    | Jump : 'r Jump.t -> 'r t
  [@@deriving sexp_of, map, variants]

  let get_virt = function
    | Virt v -> Some v
    | _ -> None
  ;;

  let map_regs i ~f = map f i
end

module Some_instr = struct
  type t = T : 'r Instr.t -> t [@@deriving sexp_of]
end

module Block = struct
  type 'r t = { instrs : ('r Instr.t, read) Vec.t } [@@deriving sexp_of, fields]

  let map_regs b ~f = { b with instrs = (Vec.map_copy & Instr.map_regs) b.instrs ~f }
end

module Graph = struct
  type 'r t = 'r Block.t Cfg.Graph.t [@@deriving sexp_of]

  let map_regs g ~f = (Cfg.Graph.map & Block.map_regs) ~f g
end

module Function = struct
  type 'r t =
    { graph : 'r Graph.t
    ; unique_name : Name.Id.t
    }
  [@@deriving sexp_of, fields]

  let map_regs fn ~f = { fn with graph = Graph.map_regs ~f fn.graph }
end

module Program = struct
  type 'r t = { functions : 'r Function.t list } [@@deriving sexp_of, fields]
end
