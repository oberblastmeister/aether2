open O
open Utils.Instr_types

module Alloc_reg = struct
  type 'r t =
    | InReg of 'r
    | Spilled
  [@@deriving sexp_of, variants]
end

module type Register = sig
  type t [@@deriving equal, compare, hash, sexp_of, enum]
end

type 'r config = { register_order : 'r list }

type 'r register =
  { sexp_of : 'r -> Sexp.t
  ; enum : 'r Data.Enum_set.enum
  ; equal : 'r -> 'r -> bool
  }

type 'r dict =
  { config : 'r config
  ; register : 'r register
  }

(* information about registers *)
module type Config = sig
  module Register : Register

  val config : Register.t config
end

type 'r allocation =
  { alloc_of_name : (Name.t, 'r Alloc_reg.t) Entity.Map.t
  ; used_registers : 'r Data.Enum_set.t
  }

let sexp_of_alloation_with ~enum f t =
  [%sexp
    ( "alloc_of_name"
    , (Entity.Map.sexp_of_t Name.sexp_of_t (Alloc_reg.sexp_of_t f) t.alloc_of_name
       : Sexp.t) )
    , ("used_registers", (Data.Enum_set.sexp_of_t_with ~enum t.used_registers : Sexp.t))]
;;

module type Algorithm = sig
  val run
    :  dict:'r dict
    -> precolored:(Name.t * 'r) list
    -> interference:Interference.t
    -> 'r allocation
end
