open O
open Utils.Instr_types
module Address = Ast.Address
module Op = Ast.Operand
module MReg = Ast.MReg
module Size = Ast.Size

module Instr = struct
  type 'r t =
    | Mov of
        { s : Size.t
        ; dst : 'r Op.t
        ; src : 'r Op.t
        }
    | MovAbs of
        { dst : 'r Op.t
        ; imm : Z.t
        }
    | MovZx of
        { dst_size : Size.t
        ; src_size : Size.t
        ; dst : 'r Op.t
        ; src : 'r Op.t
        }
    | Lea of
        { s : Size.t
        ; dst : 'r Op.t
        ; src : 'r Op.t
        }
    | Add of
        { s : Size.t
        ; dst : 'r Op.t
        ; src : 'r Op.t
        }
    | Sub of
        { s : Size.t
        ; dst : 'r Op.t
        ; src : 'r Op.t
        }
    | Imul of
        { s : Size.t
        ; dst : 'r Op.t
        ; src : 'r Op.t
        }
    | Div of
        { s : Size.t
        ; src : 'r Op.t
        }
    | Idiv of
        { s : Size.t
        ; src : 'r Op.t
        }
    | Push of
        { s : Size.t
        ; src : 'r Op.t
        }
    | Pop of
        { s : Size.t
        ; dst : 'r Op.t
        }
    | Cmp of
        { s : Size.t
        ; src1 : 'r Op.t
        ; src2 : 'r Op.t
        }
    | Test of
        { s : Size.t
        ; src1 : 'r Op.t
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
    | Cqto
  [@@deriving sexp_of, map, iter, variants]

  let iter_operands ~on_use ~on_def i =
    match i with
    | MovZx { dst; src; _ } | Mov { dst; src; _ } ->
      on_use src;
      on_def dst
    | MovAbs { dst; _ } -> on_def dst
    | Lea { dst; src; _ } ->
      on_use src;
      on_def dst
    | Add { dst; src; _ } | Sub { dst; src; _ } | Imul { dst; src; _ } ->
      on_use src;
      on_use dst;
      on_def dst
    | Div { src; _ } | Idiv { src; _ } -> on_use src
    | Push { src; _ } -> on_use src
    | Pop { dst; _ } -> on_def dst
    | Cmp { src1; src2; _ } ->
      on_use src1;
      on_use src2
    | Test { src1; src2; _ } ->
      on_use src1;
      on_use src2
    | Set { dst; _ } -> on_def dst
    | Call _ | J _ | Jmp _ | Ret | Cqto -> ()
  ;;

  let iter_op_uses i k = iter_operands ~on_use:k ~on_def:(Fn.const ()) i
  let iter_op_defs i k = iter_operands ~on_use:(Fn.const ()) ~on_def:k i

  let iter_uses i ~f =
    iter_operands
      i
      ~on_use:(fun op -> Op.iter_any_regs op ~f)
      ~on_def:(function
        | Mem m -> Ast.Address.iter_regs m ~f
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
      { s
      ; dst = Mem (Address.stack_local stack_name)
      ; src = Reg (MReg.create ~name:stack_name.name reg)
      }
  ;;

  let mov_to_reg_from_stack s reg (stack_name : Stack_slot.t) =
    Mov
      { s
      ; dst = Reg (MReg.create ~name:stack_name.name reg)
      ; src = Mem (Address.stack_local stack_name)
      }
  ;;
end

module Line = struct
  (* allow comments in instructions so we don't interfere with peephole optimization *)
  type 'r t =
    | Instr of 'r Instr.t
    | Label of string
    | Align of int
    | Global of string
    | Type of string * string
    | Comment of string
    | Section of string
    | String of string
    | Byte of char
  [@@deriving sexp_of, map, iter]

  let map_instr ~f = function
    | Instr i -> Instr (f i)
    | Label l -> Label l
    | Global g -> Global g
    | String s -> String s
    | Type (a, b) -> Type (a, b)
    | Comment c -> Comment c
    | Section s -> Section s
    | Align i -> Align i
    | Byte b -> Byte b
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
