open O
open Utils.Instr_types
module Address = Types.Address
module Op = Types.Operand
module MReg = Types.MReg

module Instr = struct
  type 'r t =
    | Mov of
        { dst : 'r Op.t
        ; src : 'r Op.t
        }
    | MovAbs of
        { dst : 'r Op.t
        ; imm : int64
        }
    | Lea of
        { dst : 'r Op.t
        ; src : 'r Op.t
        }
    | Add of
        { dst : 'r Op.t
        ; src : 'r Op.t
        }
    | Sub of
        { dst : 'r Op.t
        ; src : 'r Op.t
        }
    | Push of { src : 'r Op.t }
    | Pop of { dst : 'r Op.t }
    | Cmp of
        { src1 : 'r Op.t
        ; src2 : 'r Op.t
        }
    | Test of
        { src1 : 'r Op.t
        ; src2 : 'r Op.t
        }
    | Set of
        { cond : Cond.t
        ; dst : 'r Op.t
        }
    | Call of { src : string }
    | J of
        { cond : Cond.t
        ; src : string
        }
    | Jmp of { src : string }
    | Ret
  [@@deriving sexp_of, map, iter, variants]

  let iter_operands ~on_use ~on_def i =
    match i with
    | Mov { dst; src } ->
      on_use src;
      on_def dst
    | MovAbs { dst; _ } -> on_def dst
    | Lea { dst; src } ->
      on_use src;
      on_def dst
    | Add { dst; src } | Sub { dst; src } ->
      on_use src;
      on_use dst;
      on_def dst
    | Push { src } -> on_use src
    | Pop { dst } -> on_def dst
    | Cmp { src1; src2 } ->
      on_use src1;
      on_use src2
    | Test { src1; src2 } ->
      on_use src1;
      on_use src2
    | Set { dst; _ } -> on_def dst
    | Call _ | J _ | Jmp _ | Ret -> ()
  ;;

  let iter_op_uses i k = iter_operands ~on_use:k ~on_def:(Fn.const ()) i
  let iter_op_defs i k = iter_operands ~on_use:(Fn.const ()) ~on_def:k i

  let iter_uses i ~f =
    iter_operands
      i
      ~on_use:(fun op -> Op.iter_any_regs op ~f)
      ~on_def:(function
        | Mem m -> Types_basic.Mem.iter_regs m ~f
        | _ -> ())
  ;;

  let iter_defs i k =
    iter_operands i ~on_use:(Fn.const ()) ~on_def:(function
      | Reg r -> k r
      | _ -> ())
  ;;

  let iter_regs i ~f =
    iter_operands
      ~on_use:(fun op -> Op.iter_any_regs op ~f)
      ~on_def:(fun op -> Op.iter_any_regs op ~f)
      i
  ;;

  let map_regs i ~f = map f i

  let mov_to_stack_from_reg s (stack_name : Stack_slot.t) reg =
    Mov
      { dst = Op.mem s (Address.stack_local stack_name)
      ; src = Reg (MReg.create ~name:stack_name.name s reg)
      }
  ;;

  let mov_to_reg_from_stack s reg (stack_name : Stack_slot.t) =
    Mov
      { dst = Reg (MReg.create ~name:stack_name.name s reg)
      ; src = Op.mem s (Address.stack_local stack_name)
      }
  ;;
end

module Line = struct
  type 'r t =
    | Instr of 'r Instr.t
    | Label of string
    | Global of string
    | Type of string * string
    | Comment of string
    | SectionText
  [@@deriving sexp_of, map, iter]

  let map_instr ~f = function
    | Instr i -> Instr (f i)
    | Label l -> Label l
    | Global g -> Global g
    | Type (a, b) -> Type (a, b)
    | Comment c -> Comment c
    | SectionText -> SectionText
  ;;
end

module Program = struct
  type 'r t = ('r Line.t, read) Vec.t [@@deriving sexp_of]

  let map_instrs fn ~f =
    Vec.map_copy
      ~f:(function
        | Line.Instr i -> Line.Instr (f i)
        | other -> other)
      fn
    |> Vec.freeze
  ;;
end
