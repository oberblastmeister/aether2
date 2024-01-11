module X : sig
  type t

  val mk : t
  val id : t -> t
end = struct
  type t = int

  let mk = 0
  let id x = x
end

module Y : module type of X = struct
  type t = bool

  let mk = true
  let id x = x
end
