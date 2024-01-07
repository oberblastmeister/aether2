open O

type ('a, 'perms) t =
  { mutable size : int
  ; mutable data : 'a Option_array.t
  }

type 'a rw = ('a, Perms.Read_write.t) t

let create ?(capacity = 4) () = { size = 0; data = Option_array.create ~len:capacity }
let[@inline] unsafe_get t i = Option_array.get_some_exn t.data i

let get t i =
  if i < 0 || i >= t.size
  then raise (Invalid_argument "index out of bounds")
  else unsafe_get t i
;;

let[@inline] unsafe_set t i x = Option_array.set_some t.data i x

let set t i x =
  if i < 0 || i >= t.size
  then raise (Invalid_argument "index out of bounds")
  else unsafe_set t i x
;;

let push t x =
  (* print_s [%message "size" ~size:(t.size : int)]; *)
  if t.size = Option_array.length t.data
  then (
    let new_size = max 4 (t.size * 2) in
    let new_data = Option_array.create ~len:new_size in
    (* print_s
       [%message
        "resizing"
          ~prev_size:(t.size : int)
          ~new_size:(new_size : int)
          ~new_data_length:(Option_array.length new_data : int)]; *)
    Option_array.blit ~src:t.data ~dst:new_data ~src_pos:0 ~dst_pos:0 ~len:t.size;
    t.data <- new_data);
  (* print_s [%message "set at index" ~index:(t.size : int)]; *)
  Option_array.set_some t.data t.size x;
  t.size <- t.size + 1
;;

let pop t =
  if t.size = 0
  then None
  else (
    t.size <- t.size - 1;
    Some (Option_array.unsafe_get_some_exn t.data t.size))
;;

let pop_exn t =
  if t.size = 0
  then raise (Invalid_argument "empty Array_list")
  else (
    t.size <- t.size - 1;
    Option_array.unsafe_get_some_exn t.data t.size)
;;

let unsafe_swap t i j =
  let tmp = unsafe_get t i in
  unsafe_set t i (unsafe_get t j);
  unsafe_set t j tmp
;;

let rev_inplace t =
  let rec go i j =
    if i < j
    then (
      unsafe_swap t i j;
      go (i + 1) (j - 1))
  in
  go 0 (t.size - 1)
;;

let to_array t = Array.init t.size ~f:(unsafe_get t)
let of_array a = { size = Array.length a; data = Option_array.of_array_some a }

let of_list xs =
  let t = create () in
  List.iter xs ~f:(fun x -> push t x);
  t
;;

let to_list t =
  let a = to_array t in
  Array.fold_right a ~f:List.cons ~init:[]
;;

let fold t ~init ~f =
  let size = t.size in
  let rec go z i = if i < size then go (f z (unsafe_get t i)) (i + 1) else z in
  go init 0
;;

let fold_right t ~init ~f =
  let rec go z i = if i >= 0 then go (f (unsafe_get t i) z) (i - 1) else z in
  go init (t.size - 1)
;;

let iter t ~f =
  let size = t.size in
  let rec go i =
    if i < size
    then (
      f (unsafe_get t i);
      go (i + 1))
  in
  go 0
;;

let length t = t.size

let iteri t ~f =
  let size = t.size in
  let rec go i =
    if i < size
    then (
      f i (unsafe_get t i);
      go (i + 1))
  in
  go 0
;;

let sexp_of_t f t = Array.sexp_of_t f (to_array t)

let t_of_sexp f sexp =
  let data = Option_array.t_of_sexp f sexp in
  { size = Option_array.length data; data }
;;

let sexp_of_rw f t = sexp_of_t f t

let%test_unit _ =
  let v = create () in
  let xs = [ 1; 2; 3; 4; 5 ] in
  List.iter xs ~f:(fun x -> push v x);
  let xs_ref = ref [] in
  iter v ~f:(fun x -> xs_ref := x :: !xs_ref);
  [%test_result: int list] !xs_ref ~expect:(List.rev xs);
  [%test_result: int] (pop_exn v) ~expect:5;
  [%test_result: int] (pop_exn v) ~expect:4;
  ()
;;
