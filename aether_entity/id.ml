open! Core
include Id_intf

module T = struct
  type 'k t = int [@@deriving equal, compare, sexp, hash]

  let initial = 0
  let to_int id = id
  let global_unique = Atomic.make 0
  let of_int id = id
  let next id = id + 1
  let prev id = id - 1

  (* useful for tests *)
  let of_global_unique () = of_int (Atomic.fetch_and_add global_unique 1)
end

include T

module type S = S' with type 'k t' := 'k t

module Make () = struct
  include T

  type key [@@deriving equal, compare, hash, sexp]

  module T = struct
    type t = key T.t [@@deriving equal, compare, hash, sexp]
  end

  include T
  include Base.Comparable.Make (T)
end
