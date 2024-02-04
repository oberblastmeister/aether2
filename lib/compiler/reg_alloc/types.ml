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
module type Arch = sig
  module Register : Register
  module RegisterSet : Set with type value = Register.t
end

(* a register allocation algorithm *)
module type S = sig
  module Arch : Arch
  open Arch

  module Allocation : sig
    type t [@@deriving sexp_of]

    val find_exn : t -> Name.t -> Register.t Alloc_reg.t
    val did_use_reg : t -> Register.t -> bool

    val invariant
      :  precolored:(Name.t, Register.t) Entity.Map.t
      -> interference:Interference.t
      -> unit
  end

  val run
    :  precolored:(Name.t, Register.t) Entity.Map.t
    -> register_order:Register.t list
    -> interference:Interference.t
    -> Allocation.t Or_error.t
end

(* a register allocation algorithm depending on the architecture *)
module type Make_S = functor (Arch : Arch) -> S with module Arch := Arch
