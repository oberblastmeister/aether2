open O
open Utils.Instr_types

module Alloc_reg = struct
  type 'r t =
    | InReg of 'r
    | Spilled
  [@@deriving sexp_of, variants]
end

module type Register = sig
  type t [@@deriving equal, compare, hash, sexp_of]
end

module type Set = sig
  type t [@@deriving sexp_of]
  type value

  val create : unit -> t
  val mem : t -> value -> bool
  val add : t -> value -> unit
end

(* information about registers *)
module type Config = sig
  module Register : Register
  module RegisterSet : Set with type value = Register.t
end

(* a register allocation algorithm *)
module type S = sig
  module Config : Config
  open Config

  module Allocation : sig
    type t [@@deriving sexp_of]

    val to_iter : t -> (Name.t * Register.t Alloc_reg.t) F.Iter.t
    val find_exn : t -> Name.t -> Register.t Alloc_reg.t
    val did_use_reg : t -> Register.t -> bool
  end

  val run
    :  precolored:(Name.t, Register.t) Entity.Map.t
    -> register_order:Register.t list
    -> interference:Interference.t
    -> Allocation.t Or_error.t
end

(* a register allocation algorithm depending on the architecture *)
module type Make_S = functor (Config : Config) -> S with module Config := Config
