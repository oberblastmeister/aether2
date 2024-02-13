module type Id = sig
  type key
  type t

  val to_int : t -> int
end

module type S = sig
  include Id
  module Table : Map.S with type k = key
end
