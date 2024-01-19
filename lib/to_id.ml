module type S = sig
  type t
  type key

  val to_id : t -> key Id.t
end

module type S1 = sig
  type 'a t
  type key

  val to_id : 'a t -> key Id.t
end
