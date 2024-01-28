(* we need this file so that we can abstract over the int type *)
open! Core

type t = int [@@deriving compare, equal, hash, sexp]

let to_int id = id
let global_unique = Atomic.make 0
let of_int id = id
let next id = id + 1

(* useful for tests *)
let of_global_unique () = of_int (Atomic.fetch_and_add global_unique 1)
