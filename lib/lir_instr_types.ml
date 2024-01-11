open O
open Instr_types

module Ty = struct
  type t =
    | U1
    | U64
  [@@deriving equal, compare, hash, sexp]
end

module Value = struct
  type t =
    { name : Name.t
    ; ty : Ty.t [@equal.ignore] [@compare.ignore] [@hash.ignore]
    }
  [@@deriving equal, compare, hash, sexp, fields]
end

module CmpOp = struct
  type t = Gt [@@deriving sexp]
end

module BinOp = struct
  type t =
    | Add
    | Sub
  [@@deriving sexp]
end

module Expr = struct
  type 'v t =
    | Bin of
        { ty : Ty.t
        ; op : BinOp.t
        ; v1 : 'v
        ; v2 : 'v
        }
    | Const of
        { ty : Ty.t
        ; const : int64
        }
    | Cmp of
        { ty : Ty.t
        ; op : CmpOp.t
        ; v1 : 'v
        ; v2 : 'v
        }
    | Val of
        { ty : Ty.t
        ; v : 'v
        }
    | Alloca of { ty : Ty.t }
    | Load of
        { ty : Ty.t
        ; v : 'v
        }
  [@@deriving sexp, fold, map, iter]
end

module Instr = struct
  type 'v t =
    | Assign of
        { dst : Value.t
        ; expr : 'v Expr.t
        }
    (* | Add of
        { ty : Ty.t
        ; v1 : 'v
        ; v2 : 'v
        }
    | Sub of
        { ty : Ty.t
        ; v1 : 'v
        ; v2 : 'v
        } *)
    | Store of
        { ty : Ty.t
        ; pointer : 'v
        }
  [@@deriving sexp, fold, map, iter]
end

module BlockCall = struct
  type 'v t =
    { label : Label.t
    ; args : 'v list
    }
  [@@deriving sexp, fields, fold, map, iter]
end

module BlockArgs = struct
  type t = Value.t list [@@deriving sexp]
end

module InstrControl = struct
  type 'v t =
    | Jump of 'v BlockCall.t
    | CondJump of ('v * 'v BlockCall.t * 'v BlockCall.t)
    | Ret of 'v option
  [@@deriving sexp, fold, map, iter]
end

module InstrGeneric = struct
  type ('v, 'c) t =
    | Block_args : BlockArgs.t -> ('v, Control.e) t
    | Instr : 'v Instr.t -> ('v, Control.o) t
    | Control : 'v InstrControl.t -> ('v, Control.c) t

  let sexp_of_t (type c v) (f : v -> Sexp.t) (i : (v, c) t) =
    match i with
    | Control c -> [%sexp "Control", (InstrControl.sexp_of_t f c : Sexp.t)]
    | Block_args vs -> [%sexp "Block_args", (vs : Value.t list)]
    | Instr op -> [%sexp "Instr", (Instr.sexp_of_t f op : Sexp.t)]
  ;;
end

module SomeInstr = struct
  type 'v t = T : ('v, 'c) InstrGeneric.t -> 'v t [@@unboxed]

  let sexp_of_t f (T s) = InstrGeneric.sexp_of_t f s
end

module Block = struct
  type 'v t =
    { entry : Value.t list
    ; body : 'v Instr.t list
    ; exit : 'v InstrControl.t
    }
  [@@deriving fields]

  let sexp_of_t f ({ entry; body; exit } : 'v t) =
    [%sexp
      ("entry", (entry : Value.t list))
      , ("body", (List.sexp_of_t (Instr.sexp_of_t f) body : Sexp.t))
      , ("exit", (InstrControl.sexp_of_t f exit : Sexp.t))]
  ;;
end

module Graph = struct
  type 'v t = 'v Block.t Cfg_graph.Graph.t [@@deriving sexp_of]
end

module MutFunction = struct
  type 'v t' =
    { name : string
    ; params : Value.t list
    ; mutable graph : 'v Graph.t
    ; return_ty : Ty.t
    ; mutable unique_label : int
    ; mutable unique_name : int
    }

  type t = Value.t t'
end

module Function = struct
  type 'v t' =
    { name : string
    ; params : Value.t list
    ; graph : 'v Graph.t
    ; return_ty : Ty.t
    ; unique_label : int
    ; unique_name : int
    }
  [@@deriving sexp_of, fields]

  module Fields = Fields_of_t'
end

module Program = struct
  type 'v t' = { functions : 'v Function.t' list } [@@deriving sexp_of, fields]

  module Fields = Fields_of_t'
end
