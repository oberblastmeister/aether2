open Core
include Enum_set_intf
module F = Folds

type 'a t = Bitvec.t

let create ~enum () = Bitvec.create ~size:(enum.max + 1) false
let add ~enum t x = Bitvec.set t @@ enum.to_enum x
let remove ~enum t x = Bitvec.unset t @@ enum.to_enum x
let mem ~enum t x = Bitvec.get t @@ enum.to_enum x
let iter ~enum t ~f = Bitvec.iteri t ~f:(fun i b -> if b then f (enum.of_enum_exn i))
let count = Bitvec.count

let sexp_of_t_with ~enum t =
  iter ~enum t |> F.Iter.map ~f:enum.sexp_of |> F.Iter.to_list |> Sexp.List
;;

module Make_enum (T : Enum.S) = struct
  let enum =
    { max = T.max
    ; sexp_of = T.sexp_of_t
    ; to_enum = T.to_enum
    ; of_enum_exn = (fun i -> T.of_enum i |> Option.value_exn)
    }
  ;;
end

module Make (T : Enum.S) = struct
  type nonrec t = T.t t
  type elt = T.t

  include Make_enum (T)

  let create () = create ~enum ()
  let add = add ~enum
  let remove = remove ~enum
  let mem = mem ~enum
  let iter = iter ~enum
  let sexp_of_t = sexp_of_t_with ~enum
end

let negate = Bitvec.negate_self
