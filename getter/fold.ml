open Core

type ('b, 'a) t = T of { f : 's. 'a -> init:'s -> f:('s -> 'b -> 's) -> 's }

let of_field : ('b, 'a) Field.t -> ('b, 'a) t =
 fun field -> T { f = (fun x ~init ~f -> Field.get field x |> f init) }

let of_fn : ('a -> 'b) -> ('b, 'a) t =
 fun fn -> T { f = (fun x ~init ~f -> fn x |> f init) }

let compose : type a b c. (b, a) t -> (c, b) t -> (c, a) t =
 fun (T { f }) (T { f = g }) ->
  T
    {
      f = (fun a ~init ~f:next -> f ~init ~f:(fun z b -> g ~init:z ~f:next b) a);
    }

let ( @> ) = compose

let map : ('b -> 'c) -> ('b, 'a) t -> ('c, 'a) t =
 fun f (T { f = fold }) ->
  T { f = (fun a ~init ~f:step -> fold a ~init ~f:(fun z b -> step z (f b))) }

let rmap = map

let premap : ('a -> 'b) -> ('c, 'b) t -> ('c, 'a) t =
 fun f (T { f = fold }) ->
  T { f = (fun a ~init ~f:step -> fold (f a) ~init ~f:(fun z b -> step z b)) }

let ix t = premap (fun (_i, x) -> x) t

let reduce : ('b, 'a) t -> ('c, 'b) Reduce.t -> 'a -> 'c =
 fun (T { f = fold }) (Reduce.T (f, init, extract)) a ->
  fold ~f ~init a |> extract

let iter : ('b, 'a) t -> 'a -> f:('b -> unit) -> unit =
 fun (T { f = fold }) x ~f -> fold x ~f:(fun () b -> f b) ~init:()
