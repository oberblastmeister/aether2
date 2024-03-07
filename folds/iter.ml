open Core

type 'a t = f:('a -> unit) -> unit

let empty ~f:_ = ()
let singleton x ~f = f x

let cons x i ~f =
  f x;
  i ~f
;;

let snoc i x ~f =
  i ~f;
  f x
;;

let iter i ~f = i ~f

let fold i ~init ~f =
  let acc = ref init in
  i ~f:(fun x -> acc := f !acc x);
  !acc
;;

let filter i ~f:pred ~f:f' = i ~f:(fun x -> if pred x then f' x)
let concat_map i ~f ~f:f' = i ~f:(fun x -> f x ~f:f')

let enumerate it ~f =
  let ix = ref 0 in
  it ~f:(fun x ->
    f (!ix, x);
    incr ix);
  ()
;;

let map i ~f ~f:k = i ~f:(fun x -> k (f x))

let to_list i =
  let l = ref [] in
  i ~f:(fun x -> l := x :: !l);
  !l
;;

let length i =
  let len = ref 0 in
  i ~f:(fun _ -> incr len);
  !len
;;

let filter_map i ~f ~f:k =
  i ~f:(fun x ->
    match f x with
    | None -> ()
    | Some x -> k x)
;;

let unfoldr ~init ~f ~f:k =
  let rec loop state ~f =
    match f state with
    | None -> ()
    | Some (x, state) ->
      k x;
      loop state ~f
  in
  loop init ~f
;;

(* TODO: make this faster *)
let to_array i = Array.of_list @@ to_list i

let find_map (type a) i ~f =
  let exception Break of a in
  match
    i ~f:(fun x ->
      match f x with
      | None -> ()
      | Some x -> raise_notrace (Break x))
  with
  | () -> None
  | exception Break x -> Some x
;;

let find i ~f = find_map i ~f:(fun x -> if f x then Some x else None)
let exists i ~f = find i ~f |> Option.is_some

let int_range ~start ~stop ~f =
  for i = start to stop do
    f i
  done
;;

(* TODO: make this faster *)
let rev i ~f =
  let l = to_list i in
  List.iter (List.rev l) ~f
;;

module Infix = struct
  let ( -- ) start stop = int_range ~start ~stop
end

include Infix
