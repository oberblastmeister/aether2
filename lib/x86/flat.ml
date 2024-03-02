open O
open Utils.Instr_types

module Addr = struct
  type t = Mach_reg.t Types_basic.Address.t [@@deriving sexp_of]
end

module Op = struct
  type t =
    | Imm of int32
    | Reg of Mach_reg.t
    | Mem of Mach_reg.t Types_basic.Address.t
    | Sym of string
  [@@deriving sexp_of]
end

module Instr = struct
  type t =
    | Mov of Op.t * Op.t
    | Lea of Op.t * Op.t
    | Add of Op.t * Op.t
    | Push of Op.t
    | Pop of Op.t
    | MovAbs of Op.t * int64
    | Cmp of Op.t * Op.t
    | Test of Op.t * Op.t
    | Set of Cond.t * Op.t
    | Call of Op.t
    | J of Cond.t * Op.t
    | Jmp of Op.t
  [@@deriving sexp_of]
end

module Line = struct
  type t =
    | Instr of Instr.t
    | Comment of string
  [@@deriving sexp_of]
end
