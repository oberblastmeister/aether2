open O
open Utils.Instr_types
open Types

module type S = sig
  module Config : Config

  type t [@@deriving sexp_of]

  val create : unit -> t
  val add_register : t -> Name.t -> Config.Register.t -> unit
  val add_node : t -> Name.t -> unit
  val add_edge : t -> Name.t -> Name.t -> unit
  val neighbors : t -> Name.t -> Name.t F.Iter.t
  val nodes : t -> Name.t F.Iter.t
  val size : t -> int
end

module type Intf = sig
  module type S = S

  module Make : functor (Config : Config) -> S with module Config := Config
end
