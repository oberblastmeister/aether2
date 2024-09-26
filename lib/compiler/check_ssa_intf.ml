open O
open Utils.Instr_types

module Testing1 = struct
  type t =
    { first : int
    ; second : int
    ; third : int
    }
end

module type Config = sig
  module Instr : sig
    type t [@@deriving sexp_of]

    val iter_uses : t -> Name.t F.Iter.t
    val iter_defs : t -> Name.t F.Iter.t
  end

  module Block : sig
    type t [@@deriving sexp_of]

    val iter_instrs_forward : t -> Instr.t F.Iter.t
  end

  module Graph : sig
    type t [@@deriving sexp_of]

    val start : t -> Label.t
    val find_block_exn : t -> Label.t -> Block.t
    val iteri_blocks : t -> (Label.t * Block.t) F.Iter.t
    val to_double_graph : t -> Label.t Data.Graph.double
  end

  module Function : sig
    type t [@@deriving sexp_of]

    val to_graph : t -> Graph.t
    val params : t -> Name.t list
  end
end

module type S = sig
  type t

  val check : t -> unit Or_error.t
  val check_list : t list -> unit Or_error.t
end

module type Intf = sig
  module type Config = Config
  module type S = S

  module Make : functor (Config : Config) -> S with type t := Config.Function.t
end
