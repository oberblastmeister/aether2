type 'k t [@@deriving compare, equal, hash, sexp]

module Gen : sig
  type 'k id = 'k t
  type 'k t

  val create : unit -> 'k t
  val of_id : 'k id -> 'k t
  val to_id : 'k t -> 'k id
  val next : 'k t -> 'k id
end

val to_int : 'k t -> int
val next : 'k t -> 'k t
val of_int : int -> 'k t
val of_global_unique : unit -> 'k t
