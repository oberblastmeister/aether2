open O
open Utils.Instr_types
open Types
module Types = Types
module Interference = Interference

module type Allocation = sig
  module Config : Config
  open Config

  type t [@@deriving sexp_of]

  val to_iter : t -> (Name.t * Register.t Alloc_reg.t) F.Iter.t
  val to_spilled_iter : t -> Name.t F.Iter.t
  val find_exn : t -> Name.t -> Register.t Alloc_reg.t
  val used_registers : t -> Register.t F.Iter.t
end

module Make (Config : Config) : sig
  open Config
  module Interference = Interference

  module Constraints : sig
    type t [@@deriving sexp_of]

    val create : unit -> t
    val add : t -> Name.t -> Register.t -> unit
  end

  module Allocation : Allocation with module Config := Config

  module type Algorithm = sig
    val run
      :  precolored:(Name.t * Register.t) list
      -> interference:Interference.t
      -> Allocation.t
  end

  module Greedy : Algorithm
  module Spill_all : Algorithm
end = struct
  module Interference = Interference
  module Register = Config.Register
  module Register_enum = Data.Enum_set.Make_enum (Config.Register)

  let dict =
    { config = Config.config
    ; register =
        { sexp_of = Register.sexp_of_t
        ; equal = Register.equal
        ; enum = Register_enum.enum
        }
    }
  ;;

  module Constraints = struct
    include Constraints

    type t = Register.t Constraints.t

    let sexp_of_t = Constraints.sexp_of_t_with ~enum:Register_enum.enum
    let add = add ~enum:Register_enum.enum
  end

  module Allocation = struct
    type t = Register.t allocation

    let sexp_of_t = sexp_of_alloation_with ~enum:Register_enum.enum Register.sexp_of_t
    let find_exn t name = Name.Table.find_exn t.alloc_of_name name

    let did_use_reg t reg =
      Data.Enum_set.mem ~enum:Register_enum.enum t.used_registers reg
    ;;

    let used_registers t = Data.Enum_set.iter ~enum:Register_enum.enum t.used_registers
    let to_iter _ = todo [%here]
    let to_spilled_iter _ = todo [%here]
  end

  module type Algorithm = sig
    val run
      :  precolored:(Name.t * Register.t) list
      -> interference:Interference.t
      -> Allocation.t
  end

  module Make_algorithm (Algorithm : Types.Algorithm) : Algorithm = struct
    let run = Algorithm.run ~dict
  end

  module Greedy = Make_algorithm (Greedy)
  module Spill_all = Make_algorithm (Spill_all)
end
