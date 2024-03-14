open! O
include module type of Ast_types
module Name = Utils.Instr_types.Name
module Label = Utils.Instr_types.Label
module Control = Utils.Instr_types.Control

module Ty : sig
  include module type of Ty
end

module Value : sig
  include module type of Value
  module Hashtbl : Hashtbl.S with type key := t
  module Hash_set : Hash_set.S with type elt := t
  include Comparable.S_plain with type t := t

  val to_int : t -> int
end

module ValueMap : sig
  include module type of Entity.Map.Make (Value)
end

module Cmp_op : sig
  include module type of Cmp_op
end

module Bin_op : sig
  include module type of Bin_op
end

module Expr : sig
  include module type of Expr

  val get_ty : 'v t -> Ty.t
  val iter_uses : ('v, 'v t) F.Fold.t
end

module Some_instr : sig
  include module type of Some_instr

  val map_uses : 'v t -> f:('v -> 'u) -> 'u t
  val iter_uses : ('v, 'v t) F.Fold.t
  val iter_defs : (Value.t, 'v t) F.Fold.t
  val uses : 'v t -> 'v list
  val defs : 'v t -> Value.t list
  val to_variant : 'v t -> 'v Variant_instr.t
  val has_side_effect : 'v t -> bool
end

module Instr : sig
  include module type of Instr

  val has_side_effect : 'v t -> bool
  val map_uses : 'v t -> f:('v -> 'u) -> 'u t
  val map_defs : 'v t -> f:(Value.t -> Value.t) -> 'v t
  val iter_defs : (Value.t, 'v t) F.Fold.t
  val iter_uses : ('v, 'v t) F.Fold.t
  val to_some : 'v t -> 'v Some_instr.t
end

module Block_call : sig
  include module type of Block_call

  val map_uses : 'v t -> f:('v -> 'u) -> 'u t
end

module Control_instr : sig
  include module type of Control_instr

  val map_uses : 'v t -> f:('v -> 'u) -> 'u t
  val iter_block_calls : ('v Block_call.t, 'v t) F.Fold.t
  val block_calls : 'v t -> 'v Block_call.t list
  val map_block_calls : 'v t -> f:('v Block_call.t -> 'v Block_call.t) -> 'v t
  val to_some : 'v t -> 'v Some_instr.t
  val iter_uses : ('v, 'v t) F.Fold.t
end

module Block_args : sig
  include module type of Block_args

  val to_some : t -> 'v Some_instr.t
end

module Generic_instr : sig
  include module type of Generic_instr

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
  val to_some : ('v, 'c) t -> 'v Some_instr.t
  val iter_uses : ('v, ('v, 'c) t) F.Fold.t
  val uses : ('v, 'c) t -> 'v list
  val map_uses : ('v, 'c) t -> f:('v -> 'u) -> ('u, 'c) t
  val map_defs : ('v, 'c) t -> f:(Value.t -> Value.t) -> ('v, 'c) t
  val iter_defs : (Value.t, ('v, 'c) t) F.Fold.t
  val defs : ('v, 'c) t -> Value.t list
  val iter_block_calls : ('v Block_call.t, ('v, 'c) t) F.Fold.t
  val block_calls : ('v, 'c) t -> 'v Block_call.t list
end

module Block : sig
  include module type of Block

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
  include module type of Graph
  include Cfg.Graph.Gen_S with type 'v block := 'v Block.t

  val validate : 'v t -> unit
  val get_idoms : 'v t -> Cfg.Dominators.Idoms.t
end

module Mut_function : sig
  include module type of Mut_function

  val fresh_name : 'v t -> string -> Name.t
  val fresh_label : 'v t -> string -> Label.t
  val set_block : 'v t -> Label.t -> 'v Block.t -> unit
  val add_block_exn : 'v t -> Label.t -> 'v Block.t -> unit
end

module Named_function_ty : sig
  include module type of Named_function_ty

  val to_anon : t -> Function_ty.t
end

module Function : sig
  include module type of Function

  val map_graph : 'v t -> f:('v Graph.t -> 'u Graph.t) -> 'u t

  (* val map_blocks : 'v t -> f:('v Block.t Label.Map.t -> 'u Block.t Label.Map.t) -> 'u t *)
  val iter_instrs_forward : ('v Some_instr.t, 'v t) F.Fold.t
  val thaw : 'v t -> 'v Mut_function.t
  val freeze : 'v Mut_function.t -> 'v t
  val with_mut : 'v t -> ('v Mut_function.t -> unit) -> 'v t
end

module Program : sig
  include module type of Program

  val map_functions : 'v t -> f:('v Function.t list -> 'u Function.t list) -> 'u t
end
