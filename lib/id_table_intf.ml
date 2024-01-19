module type Gen_arg = sig
  type ('a, 'b, 'c) t

  val to_raw : ('a, 'b, 'c) t -> Raw_id.t
end

module type Arg = sig
  type t

  val to_raw : t -> Raw_id.t
end

module type Gen_S = sig
  type ('a, 'b, 'c) k
  type ('a, 'b, 'c, 'v) t

  val find : ('a, 'b, 'c, 'v) t -> ('a, 'b, 'c) k -> 'v option
  val find_exn : ('a, 'b, 'c, 'v) t -> ('a, 'b, 'c) k -> 'v
  val set : ('a, 'b, 'c, 'v) t -> key:('a, 'b, 'c) k -> data:'v -> unit
end

module type S = sig
  type k
  type 'v t

  include Gen_S with type ('a, 'b, 'c) k := k and type ('a, 'b, 'c, 'v) t := 'v t
end

module type Intf = sig
  type ('k, 'v) t

  val create : ?size:int -> (module Id.Key_type with type key = 'k) -> ('k, 'v) t
  val length : ('k, 'v) t -> int
  val find : ('k, 'v) t -> 'k -> to_id:('k -> Raw_id.t) -> 'v option
  val find_exn : ('k, 'v) t -> 'k -> to_id:('k -> Raw_id.t) -> 'v
  val set : ('k, 'v) t -> key:'k -> data:'v -> to_id:('k -> Raw_id.t) -> unit

  module Make_gen (Arg : Gen_arg) :
    Gen_S
    with type ('a, 'b, 'c) k := ('a, 'b, 'c) Arg.t
     and type ('a, 'b, 'c, 'v) t := (('a, 'b, 'c) Arg.t, 'v) t

  module Make (Arg : Arg) : S with type k := Arg.t and type 'v t := (Arg.t, 'v) t
end
