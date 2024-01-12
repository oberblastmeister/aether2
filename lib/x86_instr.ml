(* TODO: remove generic types and stuff *)
open O

open Instr_types

module Ty = struct
  type t =
    | U1
    | U64
  [@@deriving equal, compare, sexp, hash, variants]
end

module Name = Name
module Label = Label
module Control = Control
module Register = X86_register
module Jump = X86_jump

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
  type t = Reg of Register.t [@@deriving sexp]
end

module AllocReg = struct
  type t =
    | Reg of Register.t
    | Stack
  [@@deriving sexp]
end

module VReg = struct
  type t =
    | Temp of Name.t
    | PreColored of Name.t * Register.t
  [@@deriving equal, compare, sexp, hash]
end

module Address = struct
  type 'reg t =
    { base : 'reg
    ; index : 'reg
    ; scale : Scale.t
    ; displacement : int32
    }
  [@@deriving equal, compare, sexp, hash, sexp, map, fold]

  let regs_fold a k = fold (fun () reg -> k reg) () a
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
  type 'reg t =
    | Imm of int32
    | Reg of 'reg
    | Mem of 'reg Address.t
  [@@deriving equal, compare, hash, sexp, map, fold, variants]

  let reg_val_fold o k = reg_val o |> Option.map ~f:k |> ignore
  let mem_val_fold o k = mem_val o |> Option.map ~f:k |> ignore
  let mem_regs_fold o k = (mem_val_fold @> Address.regs_fold) o k

  let any_regs_fold o k =
    match o with
    | Reg r -> k r
    | Mem m -> Address.regs_fold m k
    | Imm _ -> ()
  ;;
end

module Cmp_op = struct
  type t = Gt [@@deriving equal, compare, sexp]
end

module Mov = struct
  type 'reg t =
    { s : Size.t
    ; dst : 'reg Operand.t
    ; src : 'reg Operand.t
    }
  [@@deriving sexp, map, fields]

  let defs_fold mov k = Operand.reg_val_fold mov.dst k

  let uses_fold mov k =
    Operand.mem_regs_fold mov.dst k;
    Operand.any_regs_fold mov.src k
  ;;
end

module InstrNormal = struct
  type 'reg t =
    | Lea of
        { s : Size.t
        ; dst : 'reg
        ; src : 'reg Address.t
        }
    | Add of
        { s : Size.t
        ; dst : 'reg Operand.t
        ; src1 : 'reg Operand.t
        ; src2 : 'reg Operand.t
        }
    | StoreStack of
        { s : Size.t
        ; dst : int32
        ; src : 'reg Operand.t
        }
    | LoadStack of
        { s : Size.t
        ; dst : 'reg Operand.t
        ; src : int32
        }
    | Mov of 'reg Mov.t
    | Par_mov of 'reg Mov.t list
    | Cmp of
        { s : Size.t
        ; dst : 'reg Operand.t
        ; src : 'reg Operand.t
        }
    (* for calling conventions *)
    | Def of { dst : 'reg Operand.t }
  [@@deriving sexp, map]

  let defs_fold i k =
    let module O = Operand in
    match i with
    | Def { dst; _ } -> O.reg_val_fold dst k
    | Mov mov -> O.reg_val_fold mov.dst k
    | Par_mov movs -> (FC.List.fold @> F.Fold.of_fn Mov.dst @> O.reg_val_fold) movs k
    | Add { dst; _ } -> O.reg_val_fold dst k
    | Lea { dst; _ } -> k dst
    | Cmp _ | _ -> ()
  ;;

  let uses_fold i k =
    let module O = Operand in
    match i with
    | Add { dst; src1; src2; _ } ->
      O.mem_regs_fold dst k;
      O.any_regs_fold src1 k;
      O.any_regs_fold src2 k
    | Mov mov -> Mov.uses_fold mov k
    | Par_mov movs -> (FC.List.fold @> Mov.uses_fold) movs k
    | Cmp { dst; src; _ } ->
      O.any_regs_fold dst k;
      O.any_regs_fold src k
    | Lea { src; _ } -> Address.regs_fold src k
    | Def { dst } -> O.mem_regs_fold dst k
    | _ -> ()
  ;;
end

module Block_call = struct
  type 'reg t =
    { label : Label.t
    ; args : 'reg list
    }
  [@@deriving sexp, fields, map, iter, fold]

  let uses_fold block_call k = fold (fun () reg -> k reg) () block_call
end

module Control_instr = struct
  type 'v t =
    | Jump of 'v Block_call.t
    | CondJump of (Label.t * Label.t)
  [@@deriving sexp, map, iter, fold]

  let uses_fold i k =
    match i with
    | Jump block_call -> Block_call.uses_fold block_call k
    | CondJump _ -> ()
  ;;

  let jumps_fold i k =
    match i with
    | Jump j -> k j.label
    | CondJump (j1, j2) ->
      k j1;
      k j2
  ;;
end

module Instr = struct
  module T = struct
    type ('v, 'c) t =
      | Block_args : 'v list -> ('v, Control.e) t
      | Normal : 'v InstrNormal.t -> ('v, Control.o) t
      | Control : 'v Control_instr.t -> ('v, Control.c) t
  end

  include T

  let sexp_of_t (type c v) (f : v -> Sexp.t) (i : (v, c) t) =
    match i with
    | Control c -> [%sexp "Control", (Control_instr.sexp_of_t f c : Sexp.t)]
    | Block_args regs -> [%sexp "Block_args", (List.sexp_of_t f regs : Sexp.t)]
    | Normal n -> [%sexp "Assign", (InstrNormal.sexp_of_t f n : Sexp.t)]
  ;;

  let uses_fold (type c v) (i : (v, c) t) k =
    match i with
    | Block_args _ -> ()
    | Normal instr -> InstrNormal.uses_fold instr k
    | Control c -> Control_instr.uses_fold c k
  ;;

  let defs_fold (type c v) (i : (v, c) t) k =
    match i with
    | Block_args args -> FC.List.fold args k
    | Normal instr -> InstrNormal.defs_fold instr k
    | Control _ -> ()
  ;;

  module Some = struct
    type 'v t = T : ('v, 'c) T.t -> 'v t

    let sexp_of_t f (T s) = sexp_of_t f s
  end

  let to_some i = Some.T i

  let get_control : type v. (v, Control.c) t -> v Control_instr.t = function
    | Control c -> c
    (* ocaml can't refute this for some reason? *)
    | _ -> assert false
  ;;

  let set_control : type v. (v, Control.c) t -> v Control_instr.t -> (v, Control.c) t =
    fun i c ->
    match i with
    | Control _ -> Control c
    | _ -> assert false
  ;;

  let map_control
    : type v.
      (v, Control.c) t -> f:(v Control_instr.t -> v Control_instr.t) -> (v, Control.c) t
    =
    fun i ~f -> get_control i |> f |> set_control i
  ;;

  module Dataflow = struct
    type t = VReg.t Some.t [@@deriving sexp_of]

    module Value = struct
      type t = VReg.t [@@deriving equal, compare, sexp, hash]
    end

    let uses (Some.T i) = F.Fold.reduce uses_fold F.Reduce.to_list_rev i
    let defs (Some.T i) = F.Fold.reduce defs_fold F.Reduce.to_list_rev i
  end
end

module Block = struct
  type ('v, 'c) i = ('v, 'c) Instr.t

  module T = struct
    type 'v t =
      { entry : ('v, Control.e) Instr.t
      ; body : ('v, Control.o) Instr.t list
      ; exit : ('v, Control.c) Instr.t
      }
    [@@deriving fields]

    let sexp_of_t f ({ entry; body; exit } : 'v t) =
      [%sexp
        ("entry", (Instr.sexp_of_t f entry : Sexp.t))
        , ("body", (List.map ~f:(fun i -> Instr.sexp_of_t f i) body : Sexp.t list))
        , ("exit", (Instr.sexp_of_t f exit : Sexp.t))]
    ;;
  end

  include T

  module Mapper = struct
    type ('a, 'b) t = { f : 'c. ('a, 'c) Instr.t -> ('b, 'c) Instr.t }
  end

  let map_instrs_forwards (m : _ Mapper.t) { entry; body; exit } =
    let entry = m.f entry in
    let body = List.map ~f:m.f body in
    let exit = m.f exit in
    { entry; body; exit }
  ;;

  let fold_instrs_forward ({ entry; body; exit } : _ t) ~init ~f =
    let init = f init (Instr.to_some entry) in
    let init = List.fold_left ~init ~f:(fun z i -> f z (Instr.to_some i)) body in
    f init (Instr.to_some exit)
  ;;

  let fold_instrs_backward ({ entry; body; exit } : _ t) ~init ~f =
    let init = f init (Instr.to_some exit) in
    let init =
      List.rev body |> List.fold_left ~init ~f:(fun z i -> f z (Instr.to_some i))
    in
    f init (Instr.to_some entry)
  ;;

  let instrs_forward_fold block k =
    fold_instrs_forward block ~init:() ~f:(fun () i -> k i)
  ;;

  let jumps_fold block =
    (F.Fold.of_fn (fun (b : _ t) -> b.exit)
     @> F.Fold.of_fn Instr.get_control
     @> Control_instr.jumps_fold)
      block
  ;;

  let jumps (b : _ t) = F.Fold.reduce jumps_fold F.Reduce.to_list_rev b

  module Dataflow = struct
    type t = VReg.t T.t [@@deriving sexp_of]
    type instr = VReg.t Instr.Some.t

    let fold_instrs_forward = fold_instrs_forward
    let fold_instrs_backward = fold_instrs_backward
    let jumps = jumps
  end
end

(* module Graph = struct
  type 'v t' = 'v Block.t Cfg_graph.Graph.t [@@deriving sexp_of]

  include Cfg_graph.Graph.Stuff

  module DataGraph = struct
    include MakeDataGraph (struct
        type t = VReg.t Block.t [@@deriving sexp_of]

        let jumps_fold = Block.jumps_fold
        let jumps = Block.jumps
      end)
  end

  module Dfs = Data_graph.Dfs (DataGraph)

  module AllocReg = struct
    module DataGraph = struct
      include MakeDataGraph (struct
          type t = AllocReg.t Block.t [@@deriving sexp_of]

          let jumps_fold = Block.jumps_fold
          let jumps = Block.jumps
        end)
    end

    module Dfs = Data_graph.Dfs (DataGraph)
  end

  module MachReg = struct
    module DataGraph = struct
      include MakeDataGraph (struct
          type t = MachReg.t Block.t [@@deriving sexp_of]

          let jumps_fold = Block.jumps_fold
          let jumps = Block.jumps
        end)
    end

    module Dfs = Data_graph.Dfs (DataGraph)
  end
end

module Function = struct
  type 'v t =
    { name : string
    ; body : 'v Graph.t'
    }
  [@@deriving sexp_of, fields]
end

module Program = struct
  type 'v t' = { functions : 'v Function.t list } [@@deriving sexp_of, fields]
end *)

(* module Dataflow = Cfg.MakeDataflowForBlock (Block.Dataflow)

   module DataflowInstr = struct
   (* type t = Instr.Some.t [@@deriving sexp_of]

   module Value = Instr.Value

   let uses (Instr.Some.T i) = Instr.uses i
   let defs (Instr.Some.T i) = Instr.defs i *)
   end *)

(* module Liveness = struct
   module InstrTransfer = Cfg.MakeLivenessInstrTransfer (DataflowInstr)
   module BlockTransfer = Cfg.InstrToBlockTransfer (DataflowBlock) (InstrTransfer)
   include Dataflow.MakeRun (BlockTransfer)
   end *)
