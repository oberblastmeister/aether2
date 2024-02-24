open! Core
module F = Folds

module type Gen_arg = sig
  type ('a, 'b, 'c) t

  val to_int : ('a, 'b, 'c) t -> int
end

module type Arg = sig
  type t [@@deriving sexp_of]

  val to_int : t -> int
end

module type Gen_S = sig
  type ('a, 'b, 'c) k
  type ('a, 'b, 'c, 'v) t

  val create : ?size:int -> unit -> ('a, 'b, 'c, 'v) t
  val find : ('a, 'b, 'c, 'v) t -> ('a, 'b, 'c) k -> 'v option
  val find_exn : ('a, 'b, 'c, 'v) t -> ('a, 'b, 'c) k -> 'v
  val set : ('a, 'b, 'c, 'v) t -> key:('a, 'b, 'c) k -> data:'v -> unit
  val mem : ('a, 'b, 'c, 'v) t -> ('a, 'b, 'c) k -> bool
  val update : ('a, 'b, 'c, 'v) t -> ('a, 'b, 'c) k -> f:('v option -> 'v) -> unit
  val of_list : (('a, 'b, 'c) k * 'v) list -> ('a, 'b, 'c, 'v) t
  val of_iter : ?size:int -> (('a, 'b, 'c) k * 'v) F.Iter.t -> ('a, 'b, 'c, 'v) t
  val ( .![] ) : ('a, 'b, 'c, 'v) t -> ('a, 'b, 'c) k -> 'v
  val ( .?[] ) : ('a, 'b, 'c, 'v) t -> ('a, 'b, 'c) k -> 'v option
  val ( .![]<- ) : ('a, 'b, 'c, 'v) t -> ('a, 'b, 'c) k -> 'v -> unit
end

module type S = sig
  type k
  type 'v t [@@deriving sexp_of]

  include Gen_S with type ('a, 'b, 'c) k := k and type ('a, 'b, 'c, 'v) t := 'v t
end

module type Intf = sig
  type ('k, 'v) t [@@deriving sexp_of]

  val create : ?sexp_of_key:('k -> Sexp.t) -> ?size:int -> unit -> ('k, 'v) t
  val size : ('k, 'v) t -> int
  val find : ('k, 'v) t -> 'k -> to_int:('k -> int) -> 'v option
  val find_exn : ('k, 'v) t -> 'k -> to_int:('k -> int) -> 'v
  val set : ('k, 'v) t -> key:'k -> data:'v -> to_int:('k -> int) -> unit
  val to_list : ('k, 'v) t -> ('k * 'v) list
  val iteri : ('k, 'v) t -> f:('k * 'v -> unit) -> unit
  val to_iteri : ('k, 'v) t -> ('k * 'v) F.Iter.t

  module type S = S

  module Make_gen (Arg : Gen_arg) :
    Gen_S
    with type ('a, 'b, 'c) k := ('a, 'b, 'c) Arg.t
     and type ('a, 'b, 'c, 'v) t := (('a, 'b, 'c) Arg.t, 'v) t

  module Make (Arg : Arg) : S with type k = Arg.t and type 'v t = (Arg.t, 'v) t
end
