open O
open Utils.Instr_types

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

module type S = sig
  module Register : Register
  module RegisterSet : Set

  type error = InvalidRegisterConstraint of Register.t * Register.t

  module Alloc_reg : sig
    type t =
      | InReg of Register.t
      | Spilled
    [@@deriving sexp_of, variants]
  end

  module Allocation : sig
    type t [@@deriving sexp_of]

    val find_exn : t -> Name.t -> Alloc_reg.t
    val did_use_reg : t -> Register.t -> bool
  end

  val run
    :  precolored:(Name.t, Register.t) Entity.Map.t
    -> register_order:Register.t list
    -> interference:Interference.t
    -> (Allocation.t, error) result
end

module type Intf = sig
  module Make : functor
      (Register : Register)
      (RegisterSet : Set with type value = Register.t)
      -> S with module Register := Register and module RegisterSet := RegisterSet
end
