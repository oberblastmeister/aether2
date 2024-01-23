(* TODO: remove generic types and stuff *)
open! O
open Utils.Instr_types

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
  type t = Reg of Register.t [@@deriving equal, compare, hash, sexp]
end

module AllocReg = struct
  type t =
    | Reg of Register.t
    | Stack
  [@@deriving sexp]
end

module VReg = struct
  type t =
    | Temp of { name : Name.t }
    | PreColored of
        { name : Name.t
        ; reg : Register.t [@equal.ignore] [@compare.ignore] [@hash.ignore]
        }
  [@@deriving equal, compare, sexp, hash]
end

module Reg = struct
  module T = struct
    type t =
      | VReg of VReg.t
      | MachReg of MachReg.t
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

module Size = struct
  type t =
    | Q
    | L
    | W
    | B
  [@@deriving sexp]
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

module Mov = struct
  type t =
    { s : Size.t
    ; dst : Operand.t
    ; src : Operand.t
    }
  [@@deriving sexp, fields]
end

module Block_call = struct
  type t =
    { label : Label.t
    ; args : Reg.t list
    }
  [@@deriving sexp, fields]

  let uses_fold block_call k = List.iter block_call.args ~f:k
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
    | Mov of Mov.t
    | Par_mov of Mov.t list
    | Cmp of
        { s : Size.t
        ; dst : Operand.t
        ; src : Operand.t
        }
    (* for calling conventions *)
    | Def of { dst : Operand.t }
    | Block_args of VReg.t list
    | Jump of Block_call.t
    | CondJump of
        { j : Jump.t
        ; l1 : Label.t
        ; l2 : Label.t
        }
  [@@deriving sexp, variants]

  let jumps_fold instr k =
    match instr with
    | Jump block_call -> k block_call.label
    | CondJump { l1; l2; _ } ->
      k l1;
      k l2
    | _ -> ()
  ;;
end

module Block = struct
  type t = { instrs : (Instr.t, Perms.Read.t) Vec.t } [@@deriving sexp, fields]
end

module Graph = struct
  type t = Block.t Cfg_graph.t [@@deriving sexp]

  include Cfg_graph.Make_gen (struct
      type 'a t = Block.t

      let jumps_fold =
        F.Fold.of_fn (fun g -> Vec.last g.Block.instrs)
        @> FC.Option.fold
        @> Instr.jumps_fold
      ;;
    end)
end

module Procedure = struct
  type t = { graph : Graph.t } [@@deriving sexp, fields]
end

module Program = struct
  type t = { procedures : (Procedure.t, Perms.Read.t) Vec.t } [@@deriving sexp, fields]
end
