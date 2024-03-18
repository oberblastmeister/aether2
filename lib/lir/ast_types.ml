(* TODO: use ir trees where expressions can be recursive *)
(* effectful statements cannot be inside subexpressions *)
(* unless in instruction selection when it is deemed okay*)
open! O
open Utils.Instr_types

module type Value = sig
  type t [@@deriving sexp_of, compare, hash, equal]

  include Base.Comparable.S with type t := t
end

module Ty = struct
  type t =
    | U1
    | U64
    | Void
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
        ; v1 : 'v t
        ; v2 : 'v t
        }
    | Const of
        { ty : Ty.t
        ; const : int64
        }
    | Cmp of
        { ty : Ty.t
        ; op : Cmp_op.t
        ; v1 : 'v t
        ; v2 : 'v t
        }
    | Val of 'v
  [@@deriving sexp_of, fold, map, iter]
end

module Call = struct
  type 'v t =
    { name : string
    ; args : 'v Expr.t list
    }
  [@@deriving sexp_of, fold, map, iter]
end

module Impure_expr = struct
  type 'v t =
    | Load of
        { ty : Ty.t
        ; v : 'v Expr.t
        }
    | Alloca of { ty : Ty.t }
    | Call of
        { ty : Ty.t
        ; call : 'v Call.t
        }
  [@@deriving sexp_of, fold, map, iter]
end

(* (set [x u64] (call (another)))*)
module Instr = struct
  type 'v t =
    | VoidCall of 'v Call.t
    | AssignVal of
        { dst : Value.t
        ; src : Value.t
        }
    | Assign of
        { dst : Value.t
        ; expr : 'v Expr.t
        }
    | ImpureAssign of
        { dst : Value.t
        ; expr : 'v Impure_expr.t
        }
    | Store of
        { ty : Ty.t
        ; pointer : 'v
        }
  [@@deriving sexp_of, fold, map, iter]
end

module Block_call = struct
  type 'v t =
    { label : Label.t
    ; args : 'v Expr.t list
    }
  [@@deriving sexp_of, fields, fold, map, iter]
end

module Block_args = struct
  type t = Value.t list [@@deriving sexp]
end

module Control_instr = struct
  type 'v t =
    | Jump of 'v Block_call.t
    | CondJump of ('v * 'v Block_call.t * 'v Block_call.t)
    | Ret of 'v option
  [@@deriving sexp_of, fold, map, iter]
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

  let fold (type c v) (i : (v, c) t) ~(init : 'a) ~(f : 'a -> v -> 'a) : 'a =
    match i with
    | Block_args _ -> init
    | Instr instr -> Instr.fold f init instr
    | Control c -> Control_instr.fold f init c
  ;;

  let map (type c) f (i : (_, c) t) : (_, c) t =
    match i with
    | Block_args vs -> Block_args vs
    | Instr instr -> Instr (Instr.map f instr)
    | Control c -> Control (Control_instr.map f c)
  ;;
end

module Variant_instr = struct
  type 'v t =
    | Block_args : Block_args.t -> 'v t
    | Instr : 'v Instr.t -> 'v t
    | Control : 'v Control_instr.t -> 'v t
end

module Some_instr = struct
  type 'v t = T : ('v, 'c) Generic_instr.t -> 'v t [@@unboxed]

  let sexp_of_t f (T s) = Generic_instr.sexp_of_t f s
  let map f (T s) = T (Generic_instr.map f s)
end

module Block = struct
  type 'v t =
    { entry : Value.t list
    ; body : 'v Instr.t list
    ; exit : 'v Control_instr.t
    }
  [@@deriving fields, map]

  let sexp_of_t f ({ entry; body; exit } : 'v t) =
    [%sexp
      ("entry", (entry : Value.t list))
      , ("body", (List.sexp_of_t (Instr.sexp_of_t f) body : Sexp.t))
      , ("exit", (Control_instr.sexp_of_t f exit : Sexp.t))]
  ;;
end

module Graph = struct
  type 'v t = 'v Block.t Cfg.Graph.t [@@deriving sexp_of]

  let map f graph = (Cfg.Graph.map & F.Map.of_map Block.map) graph ~f
end

module Function_ty = struct
  type t =
    { params : Ty.t list
    ; return : Ty.t
    }
  [@@deriving sexp_of]
end

(* This is used so we can retain the names
   and pretty print the ir *)
module Named_function_ty = struct
  type t =
    { params : Value.t list
    ; return : Ty.t
    }
  [@@deriving sexp_of]
end

module Mut_function = struct
  type 'v t =
    { name : string
    ; mutable graph : 'v Graph.t
    ; ty : Named_function_ty.t
    ; mutable unique_label : Label.Id.t
    ; mutable unique_name : Name.Id.t
    }
  [@@deriving sexp_of, fields]
end

module Function = struct
  type 'v t =
    { name : string
    ; graph : 'v Graph.t
    ; ty : Named_function_ty.t
    ; unique_label : Label.Id.t
    ; unique_name : Name.Id.t
    }
  [@@deriving sexp_of, fields, map]
end

module Extern = struct
  type t =
    { name : string
    ; ty : Function_ty.t
    }
  [@@deriving sexp_of]
end

(* TODO: make these map data structures *)
module Program = struct
  type 'v t =
    { funcs : 'v Function.t list
    ; externs : Extern.t list
    }
  [@@deriving sexp_of, fields, map]
end
