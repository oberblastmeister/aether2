open Core

type ('b, 'a) t = T : ('s -> 'a -> 's) * 's * ('s -> 'b) -> ('b, 'a) t

let premap (T (step, init, extract)) ~f = T ((fun s a -> step s (f a)), init, extract)
let map (T (step, init, extract)) ~f = T (step, init, Fn.compose f extract)

module Applicative = Applicative.Make2 (struct
    type nonrec ('b, 'a) t = ('b, 'a) t

    let return x = T ((fun () _ -> ()), (), fun () -> x)

    let apply (T (stepL, initL, extractL)) (T (stepR, initR, extractR)) =
      let step (xL, xR) a = stepL xL a, stepR xR a in
      let init = initL, initR in
      let extract (xL, xR) = extractL xL (extractR xR) in
      T (step, init, extract)
    ;;

    let map = `Custom map
  end)

let filter (T (step, init, extract)) ~f =
  T ((fun s a -> if f a then step s a else s), init, extract)
;;

let sum = T (( + ), 0, Fn.id)
let count = T ((fun z _ -> z + 1), 0, Fn.id)
let to_list = T ((fun z x -> x :: z), [], List.rev)

let to_map_combine empty ~combine =
  T
    ( (fun z (key, data) ->
        Map.update z key ~f:(function
          | None -> data
          | Some data' -> combine data' data))
    , empty
    , Fn.id )
;;

let to_map_with_combine empty ~f ~combine =
  T
    ( (fun z x ->
        let key, data = f x in
        Map.update z key ~f:(function
          | None -> data
          | Some data' -> combine data' data))
    , empty
    , Fn.id )
;;

let to_map_with empty ~f =
  T
    ( (fun z x ->
        let key, data = f x in
        Map.set z ~key ~data)
    , empty
    , Fn.id )
;;

let to_map empty = to_map_with empty ~f:Fn.id
let to_list_rev = T ((fun z x -> x :: z), [], Fn.id)
