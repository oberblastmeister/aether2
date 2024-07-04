open! O
module Name = Utils.Instr_types.Name
module Label = Utils.Instr_types.Label
module Control = Utils.Instr_types.Control

module type Value = sig
  type t [@@deriving sexp_of, compare, hash, equal]

  include Base.Comparable.S with type t := t
end

module Ty : sig
  type t =
    | I1
    | I64
    | Void
    | Ptr
  [@@deriving equal, compare, hash, sexp]
end

module Value : sig
  type t =
    { name : Name.t
    ; ty : Ty.t [@equal.ignore] [@compare.ignore] [@hash.ignore]
    }
  [@@deriving equal, compare, hash, sexp, fields]

  module Hashtbl : Hashtbl.S with type key := t
  module Hash_set : Hash_set.S with type elt := t
  include Comparable.S_plain with type t := t

  val to_int : t -> int
end

module ValueMap : sig
  include module type of Entity.Map.Make (Value)
end

module Cmp_op : sig
  type t =
    | Gt
    | Ge
    | Lt
    | Le
    | Eq
  [@@deriving sexp]
end

module Bin_op : sig
  type t =
    | Add
    | Sub
    | Mul
  [@@deriving sexp]
end

module Signed : sig
  type t =
    | Signed
    | Unsigned
  [@@deriving equal, compare, hash, sexp]
end

module Expr : sig
  type 'v t =
    | Bin of
        { ty : Ty.t
        ; op : Bin_op.t
        ; v1 : 'v t
        ; v2 : 'v t
        }
    | Const of
        { ty : Ty.t
        ; const : Z.t
        }
    | Cmp of
        { ty : Ty.t
        ; op : Cmp_op.t
        ; signed : Signed.t
        ; v1 : 'v t
        ; v2 : 'v t
        }
    | Val of 'v
  [@@deriving sexp_of, fold, map, iter]

  val get_val_exn : 'v t -> 'v
  val get_ty_with : ('v -> Ty.t) -> 'v t -> Ty.t
  val get_ty : Value.t t -> Ty.t
  val get_ty_exn : 'v t -> Ty.t
  val iter_uses : ('v, 'v t) F.Fold.t
end

module Call : sig
  type 'v t =
    { name : string
    ; args : 'v Expr.t list
    }
  [@@deriving sexp_of, fold, map, iter]
end

module Impure_expr : sig
  type 'v t =
    | Udiv of
        { ty : Ty.t
        ; v1 : 'v Expr.t
        ; v2 : 'v Expr.t
        }
    | Idiv of
        { ty : Ty.t
        ; v1 : 'v Expr.t
        ; v2 : 'v Expr.t
        }
    | Load of
        { ty : Ty.t
        ; ptr : 'v Expr.t
        }
    | Alloca of { size : int32 }
    | Call of
        { ty : Ty.t
        ; call : 'v Call.t
        }
  [@@deriving sexp_of, fold, map, iter]

  val get_ty : 'v t -> Ty.t
  val iter_uses : ('v, 'v t) F.Fold.t
end

module Block_args : sig
  type t = Value.t list [@@deriving sexp]
end

module Block_call : sig
  type 'v t =
    { label : Label.t
    ; args : 'v Expr.t list
    }
  [@@deriving sexp_of, fields, fold, map, iter]

  val map_uses : 'v t -> f:('v -> 'u) -> 'u t
end

module Control_instr : sig
  type 'v t =
    | Jump of 'v Block_call.t
    | CondJump of ('v Expr.t * 'v Block_call.t * 'v Block_call.t)
    | Ret of 'v Expr.t option
  [@@deriving sexp_of, fold, map, iter]

  val map_uses : 'v t -> f:('v -> 'u) -> 'u t
  val iter_block_calls : ('v Block_call.t, 'v t) F.Fold.t
  val block_calls : 'v t -> 'v Block_call.t list
  val map_block_calls : 'v t -> f:('v Block_call.t -> 'v Block_call.t) -> 'v t

  (* val to_some : 'v t -> 'v Some_instr.t *)
  val iter_uses : ('v, 'v t) F.Fold.t
end

module Instr : sig
  type 'v t =
    | VoidCall of 'v Call.t
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
        ; ptr : 'v Expr.t
        ; expr : 'v Expr.t
        }
  [@@deriving sexp_of, fold, map, iter]

  val has_side_effect : 'v t -> bool
  val map_uses : 'v t -> f:('v -> 'u) -> 'u t
  val map_defs : 'v t -> f:(Value.t -> Value.t) -> 'v t
  val iter_defs : (Value.t, 'v t) F.Fold.t
  val iter_uses : ('v, 'v t) F.Fold.t
  (* val to_some : 'v t -> 'v Some_instr.t *)
end

module Generic_instr : sig
  type ('v, 'c) t =
    | Block_args : Block_args.t -> ('v, Control.e) t
    | Instr : 'v Instr.t -> ('v, Control.o) t
    | Control : 'v Control_instr.t -> ('v, Control.c) t

  val sexp_of_t : ('v -> Sexp.t) -> ('v, 'c) t -> Sexp.t
  val fold : ('v, 'c) t -> init:'a -> f:('a -> 'v -> 'a) -> 'a
  val map : ('v, 'c) t -> f:('v -> 'u) -> ('u, 'c) t
  val get_control : ('v, Control.c) t -> 'v Control_instr.t

  val map_control
    :  ('v, Control.c) t
    -> f:('v Control_instr.t -> 'u Control_instr.t)
    -> ('u, Control.c) t

  val get_instr : ('v, Control.o) t -> 'v Instr.t
  val map_instr : ('v, Control.o) t -> f:('v Instr.t -> 'u Instr.t) -> ('u, Control.o) t
  val block_args_exn : ('v, Control.e) t -> Block_args.t

  val map_block_args
    :  ('v, Control.e) t
    -> f:(Block_args.t -> Block_args.t)
    -> ('v, Control.e) t

  val map : ('v, 'c) t -> f:('v -> 'u) -> ('u, 'c) t

  (* val to_some : ('v, 'c) t -> 'v Some_instr.t *)
  val iter_uses : ('v, ('v, 'c) t) F.Fold.t
  val uses : ('v, 'c) t -> 'v list
  val map_uses : ('v, 'c) t -> f:('v -> 'u) -> ('u, 'c) t
  val map_defs : ('v, 'c) t -> f:(Value.t -> Value.t) -> ('v, 'c) t
  val iter_defs : (Value.t, ('v, 'c) t) F.Fold.t
  val defs : ('v, 'c) t -> Value.t list
  val iter_block_calls : ('v Block_call.t, ('v, 'c) t) F.Fold.t
  val block_calls : ('v, 'c) t -> 'v Block_call.t list
end

module Variant_instr : sig
  type 'v t =
    | Block_args : Block_args.t -> 'v t
    | Instr : 'v Instr.t -> 'v t
    | Control : 'v Control_instr.t -> 'v t
end

module Some_instr : sig
  type 'v t = T : ('v, 'c) Generic_instr.t -> 'v t [@@unboxed]

  val sexp_of_t : ('v -> Sexp.t) -> 'v t -> Sexp.t
  val map_uses : 'v t -> f:('v -> 'u) -> 'u t
  val iter_uses : ('v, 'v t) F.Fold.t
  val iter_defs : (Value.t, 'v t) F.Fold.t
  val uses : 'v t -> 'v list
  val defs : 'v t -> Value.t list
  val to_variant : 'v t -> 'v Variant_instr.t
  val has_side_effect : 'v t -> bool
end

module Block : sig
  type 'v t =
    { entry : Value.t list
    ; body : 'v Instr.t list
    ; exit : 'v Control_instr.t
    }
  [@@deriving fields, map, sexp_of]

  val map_exit : 'v t -> f:('v Control_instr.t -> 'v Control_instr.t) -> 'v t

  module Mapper : sig
    type ('a, 'b) t = { f : 'c. ('a, 'c) Generic_instr.t -> ('b, 'c) Generic_instr.t }
  end

  val map_instrs_forwards : ('v, 'u) Mapper.t -> 'v t -> 'u t
  val iter_instrs_forward : ('v Some_instr.t, 'v t) F.Fold.t
  val iter_instrs_backward : ('v Some_instr.t, 'v t) F.Fold.t
  val iter_jumps : (Label.t, 'v t) F.Fold.t
  val jumps : 'v t -> Label.t list
end

module Graph : sig
  type 'v t = 'v Block.t Cfg.Graph.t [@@deriving sexp_of]

  include Cfg.Graph.Gen_S with type 'v block := 'v Block.t

  val validate : 'v t -> unit
  val get_idoms : 'v t -> Cfg.Dominators.Idoms.t
end

module Function_ty : sig
  type t =
    { params : Ty.t list
    ; return : Ty.t
    }
  [@@deriving sexp_of]
end

module Named_function_ty : sig
  type t =
    { params : Value.t list
    ; return : Ty.t
    }
  [@@deriving sexp_of]

  val to_anon : t -> Function_ty.t
end

module Mut_function : sig
  type 'v t =
    { name : string
    ; mutable graph : 'v Graph.t
    ; ty : Named_function_ty.t
    ; mutable unique_label : Label.Id.t
    ; mutable unique_name : Name.Id.t
    }
  [@@deriving sexp_of, fields]

  val fresh_name : 'v t -> string -> Name.t
  val fresh_label : 'v t -> string -> Label.t
  val set_block : 'v t -> Label.t -> 'v Block.t -> unit
  val add_block_exn : 'v t -> Label.t -> 'v Block.t -> unit
end

module Extern : sig
  type t =
    { name : string
    ; ty : Function_ty.t
    }
  [@@deriving sexp_of]
end

module Function : sig
  type 'v t =
    { name : string
    ; graph : 'v Graph.t
    ; ty : Named_function_ty.t
    ; unique_label : Label.Id.t
    ; unique_name : Name.Id.t
    }
  [@@deriving sexp_of, fields, map]

  val map_graph : 'v t -> f:('v Graph.t -> 'u Graph.t) -> 'u t

  (* val map_blocks : 'v t -> f:('v Block.t Label.Map.t -> 'u Block.t Label.Map.t) -> 'u t *)
  val iter_blocks : 'v t -> 'v Block.t F.Iter.t
  val iter_instrs_forward : ('v Some_instr.t, 'v t) F.Fold.t
  val thaw : 'v t -> 'v Mut_function.t
  val freeze : 'v Mut_function.t -> 'v t
  val with_mut : 'v t -> ('v Mut_function.t -> unit) -> 'v t
end

module Program : sig
  type 'v t =
    { funcs : 'v Function.t list
    ; externs : Extern.t list
    }
  [@@deriving sexp_of, fields, map]

  val map_functions : 'v t -> f:('v Function.t list -> 'u Function.t list) -> 'u t
end
