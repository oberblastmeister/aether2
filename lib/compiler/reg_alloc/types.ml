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

  val order : t list
end

module type Set = sig
  type t [@@deriving sexp_of]
  type elt

  val create : unit -> t
  val mem : t -> elt -> bool
  val add : t -> elt -> unit
end

module type VReg = sig
  type t [@@deriving sexp_of]

  val to_raw : t -> Entity.Raw_id.t
end

(* information about registers *)
module type Config = sig
  module Register : Register
  module RegisterSet : Set with type elt = Register.t
end

module type Allocation = sig
  module Config : Config
  open Config

  type t [@@deriving sexp_of]

  val to_iter : t -> (Name.t * Register.t Alloc_reg.t) F.Iter.t
  val to_spilled_iter : t -> Name.t F.Iter.t
  val find_exn : t -> Name.t -> Register.t Alloc_reg.t
  val did_use_reg : t -> Register.t -> bool
end

(* module type Arg = sig
   module Config : Config
   open Config
   end *)

type ('r, 'rs) allocation =
  { alloc_of_name : (Name.t, 'r Alloc_reg.t) Entity.Map.t
  ; used_registers : 'rs
  }
[@@deriving sexp_of]

module NameMap = Entity.Map.Make (Name)

module Make_allocation (Config : Config) = struct
  open Config

  type t = (Register.t, RegisterSet.t) allocation [@@deriving sexp_of]

  let find_exn t name = NameMap.find_exn t.alloc_of_name name
  let did_use_reg t reg = RegisterSet.mem t.used_registers reg
  let to_iter _ = todo ()
  let to_spilled_iter _ = todo ()
end

module Make_arg (Config : Config) = struct
  module Config = Config
  open Config
  module Allocation = Make_allocation (Config)
end

(* allocation is a parameter because we want it abstract in the make interface *)
module Make_S (Config : Config) (Allocation : Sig.T) = struct
  module type S = sig
    open Config

    val run
      :  precolored:(Name.t, Register.t) Entity.Map.t
      -> interference:Interference.t
      -> Allocation.t Or_error.t
  end
end

(* a register allocation algorithm depending on the architecture *)
module type Make_S = functor (Config : Config) ->
  Make_S(Config)(Make_allocation(Config)).S
