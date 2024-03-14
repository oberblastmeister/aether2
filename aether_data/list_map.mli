open Core

type ('k, 'v) t [@@deriving sexp_of, compare, equal]

val empty : ('k, _) Comparator.Module.t -> ('k, 'v) t
val of_alist_exn : ('k, _) Comparator.Module.t -> ('k * 'v) list -> ('k, 'v) t
val to_alist : ('k, 'v) t -> ('k * 'v) list
val find : ('k, 'v) t -> 'k -> 'v option
val find_exn : ('k, 'v) t -> 'k -> 'v
val add_exn : ('k, 'v) t -> key:'k -> data:'v -> ('k, 'v) t
