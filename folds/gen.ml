open Core
module M = Immediate_option

(* church encoded ('a * 's) option *)
type ('s, 'a) step = { f : 'r. 'r -> ('a -> 's -> 'r) -> 'r } [@@unboxed]
type 'a t = T : ('s * ('s -> ('s, 'a) step)) -> 'a t

let unfold step f = failwith ""
let of_seq (seq : _ Sequence.t) = failwith ""
