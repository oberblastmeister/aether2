open! O

module type Option_array_getters = sig
  val unsafe_get_some_assuming_some : 'a Option_array.t -> int -> 'a
  val unsafe_set_some : 'a Option_array.t -> int -> 'a -> unit
end

module Safe_getters = struct
  let unsafe_get_some_assuming_some = Option_array.get_some_exn
  let unsafe_set_some = Option_array.set_some
end

module Unsafe_getters = struct
  let unsafe_get_some_assuming_some = Option_array.unsafe_get_some_assuming_some
  let unsafe_set_some = Option_array.unsafe_set_some
end

module Make_raw (A : Option_array_getters) = struct
  type 'a t =
    { mutable size : int
    ; mutable data : 'a Option_array.t
    ; mutable frozen : bool
    }

  let create ?(capacity = 4) () =
    { size = 0; data = Option_array.create ~len:capacity; frozen = false }
  ;;

  let freeze t =
    t.frozen <- true;
    t
  ;;

  let _ = Array.Permissioned.copy

  let copy t =
    let t' = create ~capacity:t.size () in
    Option_array.blit ~src:t.data ~dst:t'.data ~src_pos:0 ~dst_pos:0 ~len:t.size;
    t'
  ;;

  let[@inline] unsafe_get t i = A.unsafe_get_some_assuming_some t.data i

  let get t i =
    if i < 0 || i >= t.size then invalid_arg "index out of bounds" else unsafe_get t i
  ;;

  let[@inline] unsafe_set t i x = A.unsafe_set_some t.data i x
  let[@inline] check_not_frozen t = if t.frozen then invalid_arg "vec was frozen" else ()

  let set t i x =
    check_not_frozen t;
    if i < 0 || i >= t.size
    then raise (Invalid_argument "index out of bounds")
    else unsafe_set t i x
  ;;

  let push t x =
    check_not_frozen t;
    if t.size = Option_array.length t.data
    then (
      let new_size = max 4 (t.size * 2) in
      let new_data = Option_array.create ~len:new_size in
      Option_array.blit ~src:t.data ~dst:new_data ~src_pos:0 ~dst_pos:0 ~len:t.size;
      t.data <- new_data);
    A.unsafe_set_some t.data t.size x;
    t.size <- t.size + 1
  ;;

  let pop t =
    check_not_frozen t;
    if t.size = 0
    then None
    else (
      t.size <- t.size - 1;
      Some (A.unsafe_get_some_assuming_some t.data t.size))
  ;;

  let pop_exn t =
    check_not_frozen t;
    if t.size = 0
    then raise (Invalid_argument "empty Array_list")
    else (
      t.size <- t.size - 1;
      A.unsafe_get_some_assuming_some t.data t.size)
  ;;

  let unsafe_swap t i j =
    let tmp = unsafe_get t i in
    unsafe_set t i (unsafe_get t j);
    unsafe_set t j tmp
  ;;

  let rev_inplace t =
    check_not_frozen t;
    let rec go i j =
      if i < j
      then (
        unsafe_swap t i j;
        go (i + 1) (j - 1))
    in
    go 0 (t.size - 1)
  ;;

  let to_array t = Array.init t.size ~f:(unsafe_get t)

  let of_array a =
    { size = Array.length a; data = Option_array.of_array_some a; frozen = false }
  ;;

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

  let compare f t t' =
    let size_ord = Int.compare t.size t'.size in
    if size_ord <> 0
    then size_ord
    else (
      let rec go i =
        if i < t.size
        then (
          let x = unsafe_get t i in
          let y = unsafe_get t' i in
          let ord = f x y in
          if ord <> 0 then ord else go (i + 1))
        else 0
      in
      go 0)
  ;;

  let equal f t t' =
    if t.size = t'.size
    then (
      let rec go i =
        if i < t.size
        then (
          let x = unsafe_get t i in
          let y = unsafe_get t' i in
          if f x y then go (i + 1) else false)
        else true
      in
      go 0)
    else false
  ;;

  let sexp_of_t f t = Array.sexp_of_t f (to_array t)

  let t_of_sexp f sexp =
    let data = Option_array.t_of_sexp f sexp in
    { size = Option_array.length data; data; frozen = false }
  ;;

  let hash_fold_t hash_fold_a state t =
    let state = Int.hash_fold_t state t.size in
    let rec go state i =
      if i < t.size then go (hash_fold_a state (unsafe_get t i)) (i + 1) else state
    in
    go state 0
  ;;

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

  include Utils.Make_quickcheck_list_conv (struct
      type nonrec 'a t = 'a t

      let of_list = of_list
      let to_list = to_list
    end)
end

module Raw = Make_raw (Safe_getters)
include Raw

type ('a, -'perms) t = 'a Raw.t

let equal f _ = Raw.equal f
let compare f _ = Raw.compare f
let hash_fold_t f _ = Raw.hash_fold_t f
let sexp_of_t f _ t = Raw.sexp_of_t f t
let t_of_sexp f _ sexp = Raw.t_of_sexp f sexp

let%expect_test _ =
  let t = of_list [ 1; 2; 3; 4; 5 ] in
  print_s [%sexp (t : (int, _) t)];
  [%expect {| (1 2 3 4 5) |}]
;;
