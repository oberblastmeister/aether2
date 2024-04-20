open O
open Ast
open Utils.Instr_types

type t [@@deriving sexp_of]

val create : Stack_instr.t list -> t
val start_offset : t -> int32 -> int32
val end_offset : t -> int32 -> int32
val local_offset : t -> Stack_slot.t -> int32
val size : t -> int32
