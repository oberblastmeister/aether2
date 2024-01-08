open Core

type ('b, 'a) t = 'a -> 'b Iter.t

let[@inline] of_field field k x = Field.get field x |> k
let[@inline] of_fn f x k = f x |> k
let[@inline] compose f g a k = f a (fun b -> g b k)
let[@inline] ( @> ) = compose
let[@inline] map f fold a k = fold a (fun b -> k (f b))
let[@inline] postmap = map
let[@inline] premap f fold a k = fold (f a) k
let[@inline] no_ix t = premap (fun (_i, x) -> x) t
let[@inline] no_ix2 t = no_ix (no_ix t)
let[@inline] no_ix3 t = no_ix (no_ix (no_ix t))
let[@inline] ix fold (i, a) k = fold a (fun b -> k (i, b))
let[@inline] ix2 t = ix (ix t)
let[@inline] ix3 t = ix (ix (ix t))
let[@inline] dup t = map (fun x -> x, x) t
let[@inline] foldi fold x ~init ~f = Iter.foldi ~init ~f (fold x)
let[@inline] fold fold x ~init ~f = Iter.fold ~init ~f (fold x)
let[@inline] iter fold x ~f = Iter.iter ~f (fold x)

let[@inline] reduce fold (Reduce.T (step, init, extract)) x =
  Iter.fold ~f:step ~init (fold x) |> extract
;;
