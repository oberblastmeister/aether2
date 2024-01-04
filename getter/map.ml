open Core

type ('t, 's, 'b, 'a) t = 'a -> f:('s -> 't) -> 'b
type ('s, 'a) t' = ('s, 's, 'a, 'a) t

let compose :
      'v 'u 't 's 'b 'a.
      ('t, 's, 'b, 'a) t -> ('v, 'u, 't, 's) t -> ('v, 'u, 'b, 'a) t =
 fun mf mg a ~f -> mf ~f:(fun s -> mg ~f s) a

let ( & ) = compose
let of_field field ~f = Field.map field ~f
