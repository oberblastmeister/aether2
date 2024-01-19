open! O

type 'k t = int [@@deriving compare, equal, hash, sexp]

module Gen = struct
  type 'k id = 'k t
  type 'k t = int ref

  let create () = ref 0
  let of_id = ref
  let to_id t = !t

  let next t =
    let id = !t in
    incr t;
    id
  ;;
end

let to_int id = id
let global_unique = Atomic.make 0
let of_int id = id
let next id = id + 1

(* useful for tests *)
let of_global_unique () = of_int (Atomic.fetch_and_add global_unique 1)
