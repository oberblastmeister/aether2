open O
open Utils.Instr_types

type t [@@deriving sexp_of]

val create : unit -> t
val add_edge : t -> Name.t -> Name.t -> unit
val neighbors : t -> Name.t -> Name.t F.Iter.t
