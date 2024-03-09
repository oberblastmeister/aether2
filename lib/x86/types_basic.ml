(* all register types must have sizes because they may be used inside addresses *)
open! O
open Utils.Instr_types

module Size = struct
  type t = Q [@@deriving equal, compare, hash, sexp]
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

module MReg = struct
  type t =
    { s : Size.t
    ; name : string option
    ; reg : Mach_reg.t
    }
  [@@deriving equal, compare, hash]

  let sexp_of_t { s; name; reg } =
    match name with
    | None -> [%sexp (reg : Mach_reg.t)]
    | Some name -> [%sexp (name : string), (reg : Mach_reg.t)]
  ;;

  let create ?name s reg = { s; name; reg }
end

module VReg = struct
  (* make this a variant so we can have unnamed virtual registers *)
  module T = struct
    type t =
      { s : Size.t [@equal.ignore] [@compare.ignore] [@hash.ignore]
      ; name : Name.t
      }
    [@@deriving equal, compare, sexp, hash, fields]
  end

  include T
  include Comparable.Make (T)
end

module AReg = struct
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

module Imm = struct
  type t =
    | Int of int32
    | Stack of Stack_off.t
  [@@deriving sexp_of, map, fold]
end

(* used sized registers here and remove sizes on all registers *)
module Address = struct
  module Base = struct
    type 'r t =
      | None
      | Reg of 'r
      | Rip
    [@@deriving sexp_of, variants, map, fold, iter]
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
    [@@deriving sexp_of, variants, map, fold, iter]
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
end

module Mem = struct
  type 'r t =
    { size : Size.t
    ; addr : 'r Address.t
    }
  [@@deriving sexp_of, map, fold, iter]

  let iter_regs i ~f:k = iter k i
  let map_addr t ~f = { t with addr = f t.addr }
end

module Operand = struct
  type 'r t =
    | Imm of Imm.t
    | Reg of 'r
    | Mem of 'r Mem.t
  [@@deriving sexp_of, variants, map, fold, iter]

  let mem size addr = Mem { size; addr }
end
(*
   module GOperand = struct
   type imm = Imm_
   type reg = Reg_
   type mem = Mem_

   type ('r, 'op) t =
   | Imm : Imm.t -> ('r, imm) t
   | Reg : 'r -> ('r, reg) t
   | Mem : 'r Address.t -> ('r, mem) t

   let to_operand (type op) (t : (_, op) t) =
   match t with
   | Imm imm -> Operand.Imm imm
   | Reg reg -> Reg reg
   | Mem addr -> Mem addr
   ;;

   let mem_val = function
   | Mem addr -> addr
   ;;

   let imm_val = function
   | Imm imm -> imm
   ;;

   let reg_val = function
   | Reg reg -> reg
   ;;
   end *)

module Cmp_op = struct
  type t = Gt [@@deriving equal, compare, sexp]
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

module VInstr = struct
  (* TODO: allow operands in par mov source *)
  (* TODO: put stack operations in separate type *)
  type 'r t =
    (* for calling conventions*)
    (* | ReserveStackEnd of { size : int32 } *)
    (* | ReserveStackLocal of
        { name : Name.t
        ; size : int32
        } *)
    (* for ssa *)
    | Block_args of 'r list
    (* | Par_mov of ('r Precolored.t * 'r Precolored.t) list *)
    | Par_mov of ('r * 'r) list
  [@@deriving sexp_of, variants, map]
end

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
    | Ret of 'r Operand.t option
  [@@deriving sexp_of, fold, map, iter]
end

module Instr = struct
  type 'r t =
    | NoOp
    | Mov of
        { dst : 'r Operand.t
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
    | Push of { src : 'r }
    | Pop of { dst : 'r }
    | MovAbs of
        { dst : 'r Operand.t
        ; imm : int64
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
        ; dst_reg : Mach_reg.t
        ; dst : 'r
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

module Some_instr = struct
  type t = T : 'r Instr.t -> t [@@deriving sexp_of]
end

module Block = struct
  type 'r t = { instrs : ('r Instr.t, read) Vec.t } [@@deriving sexp_of, fields]

  let map_regs b ~f = { b with instrs = (Vec.map_copy & Instr.map_regs) b.instrs ~f }
  let map_instrs b ~f = { b with instrs = Vec.map_copy b.instrs ~f }
end

module Graph = struct
  type 'r t = 'r Block.t Cfg.Graph.t [@@deriving sexp_of]

  let map_regs g ~f = (Cfg.Graph.map & Block.map_regs) ~f g
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
end

module Program = struct
  type 'r t = { functions : 'r Function.t list } [@@deriving sexp_of, fields]
end
