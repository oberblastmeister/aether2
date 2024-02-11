open O
open Types
open Utils.Instr_types

type t [@@deriving sexp_of]

val create : _ Function.t -> t
val end_offset : t -> int32 -> int32
val local_offset : t -> Name.t -> int32
