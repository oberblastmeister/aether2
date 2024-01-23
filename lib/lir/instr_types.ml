open! O
open Utils.Instr_types

module type Value = sig
  type t [@@deriving sexp_of, compare, hash, equal]

  include Comparable.S with type t := t
end

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

module Cmp_op = struct
  type t = Gt [@@deriving sexp]
end

module Bin_op = struct
  type t =
    | Add
    | Sub
  [@@deriving sexp]
end

module Expr = struct
  type 'v t =
    | Bin of
        { ty : Ty.t
        ; op : Bin_op.t
        ; v1 : 'v
        ; v2 : 'v
        }
    | Const of
        { ty : Ty.t
        ; const : int64
        }
    | Cmp of
        { ty : Ty.t
        ; op : Cmp_op.t
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
    | Store of
        { ty : Ty.t
        ; pointer : 'v
        }
  [@@deriving sexp, fold, map, iter]
end

module Block_call = struct
  type 'v t =
    { label : Label.t
    ; args : 'v list
    }
  [@@deriving sexp, fields, fold, map, iter]
end

module Block_args = struct
  type t = Value.t list [@@deriving sexp]
end

module Control_instr = struct
  type 'v t =
    | Jump of 'v Block_call.t
    | CondJump of ('v * 'v Block_call.t * 'v Block_call.t)
    | Ret of 'v option
  [@@deriving sexp, fold, map, iter]
end

module Generic_instr = struct
  type ('v, 'c) t =
    | Block_args : Block_args.t -> ('v, Control.e) t
    | Instr : 'v Instr.t -> ('v, Control.o) t
    | Control : 'v Control_instr.t -> ('v, Control.c) t

  let sexp_of_t (type c v) (f : v -> Sexp.t) (i : (v, c) t) =
    match i with
    | Control c -> [%sexp "Control", (Control_instr.sexp_of_t f c : Sexp.t)]
    | Block_args vs -> [%sexp "Block_args", (vs : Value.t list)]
    | Instr op -> [%sexp "Instr", (Instr.sexp_of_t f op : Sexp.t)]
  ;;
end

module Some_instr = struct
  type 'v t = T : ('v, 'c) Generic_instr.t -> 'v t [@@unboxed]

  let sexp_of_t f (T s) = Generic_instr.sexp_of_t f s
end

module Block = struct
  type 'v t =
    { entry : Value.t list
    ; body : 'v Instr.t list
    ; exit : 'v Control_instr.t
    }
  [@@deriving fields]

  let sexp_of_t f ({ entry; body; exit } : 'v t) =
    [%sexp
      ("entry", (entry : Value.t list))
      , ("body", (List.sexp_of_t (Instr.sexp_of_t f) body : Sexp.t))
      , ("exit", (Control_instr.sexp_of_t f exit : Sexp.t))]
  ;;
end

module Graph = struct
  type 'v t = 'v Block.t Cfg_graph.t [@@deriving sexp_of]
end

module Mut_function = struct
  type 'v t =
    { name : string
    ; params : Value.t list
    ; mutable graph : 'v Graph.t
    ; return_ty : Ty.t
    ; mutable unique_label : Label.Id.t
    ; mutable unique_name : Name.Id.t
    }
  [@@deriving sexp_of, fields]
end

module Function = struct
  type 'v t =
    { name : string
    ; params : Value.t list
    ; graph : 'v Graph.t
    ; return_ty : Ty.t
    ; unique_label : Label.Id.t
    ; unique_name : Name.Id.t
    }
  [@@deriving sexp_of, fields]
end

module Program = struct
  type 'v t = { functions : 'v Function.t list } [@@deriving sexp_of, fields]
end
