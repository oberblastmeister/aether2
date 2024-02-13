open Core
include Enum_set_intf
module F = Folds

type 'a t = Bitvec.t

let create (type a) ~enum:(module Enum : Enum.S with type t = a) =
  Bitvec.create ~size:(Enum.max + 1) false
;;

let add (type a) ~enum:(module Enum : Enum.S with type t = a) t x =
  Bitvec.set t @@ Enum.to_enum x
;;

let remove (type a) ~enum:(module Enum : Enum.S with type t = a) t x =
  Bitvec.unset t @@ Enum.to_enum x
;;

let mem (type a) ~enum:(module Enum : Enum.S with type t = a) t x =
  Bitvec.get t @@ Enum.to_enum x
;;

let iter (type a) ~enum:(module Enum : Enum.S with type t = a) t ~f =
  Bitvec.iteri t ~f:(fun i b -> if b then f (Enum.of_enum i |> Option.value_exn))
;;

module Make (T : Enum.S) = struct
  type nonrec t = T.t t
  type elt = T.t

  let enum = (module T : Enum.S with type t = T.t)
  let create () = create ~enum
  let add = add ~enum
  let remove = remove ~enum
  let mem = mem ~enum
  let iter = iter ~enum

  let sexp_of_t t =
    (fun f -> iter t ~f) |> F.Iter.map ~f:T.sexp_of_t |> F.Iter.to_list |> Sexp.List
  ;;
end

let negate = Bitvec.negate_self
