open! Core
module F = Folds

module type Arg = sig
  type t [@@deriving sexp_of]

  val to_int : t -> int
end

module type S = sig
  type k
  type 'v t [@@deriving sexp_of]

  val create : ?size:int -> unit -> 'v t
  val find : 'v t -> k -> 'v option
  val remove : 'v t -> k -> unit
  val find_exn : 'v t -> k -> 'v
  val set : 'v t -> key:k -> data:'v -> unit
  val add_exn : 'v t -> key:k -> data:'v -> unit
  val mem : 'v t -> k -> bool
  val update : 'v t -> k -> f:('v option -> 'v) -> unit
  val of_list : (k * 'v) list -> 'v t
  val of_iter : ?size:int -> (k * 'v) F.Iter.t -> 'v t

  val of_iter_accum
    :  ?size:int
    -> (k * 'v) F.Iter.t
    -> init:'acc
    -> f:('acc -> 'v -> 'acc)
    -> 'acc t

  val ( .![] ) : 'v t -> k -> 'v
  val ( .?[] ) : 'v t -> k -> 'v option
  val ( .![]<- ) : 'v t -> k -> 'v -> unit
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
  val clear : _ t -> unit

  module type S = S

  module Make (Arg : Arg) : S with type k = Arg.t and type 'v t = (Arg.t, 'v) t
end
