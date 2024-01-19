type 'k t = 'k Id.t ref

module type Key = sig
  type key
end

let create _ = ref @@ Id.of_int 0
let of_id = ref
let to_id t = !t

let next t =
  let id = !t in
  t := Id.next id;
  id
;;
