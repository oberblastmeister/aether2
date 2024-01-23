open! O

module type Option_array_getters = sig
  val unsafe_get_some_assuming_some : 'a Option_array.t -> int -> 'a
  val unsafe_set_some : 'a Option_array.t -> int -> 'a -> unit
end

module A = Array_ops.Option_array.Safe_ops

module Raw = struct
  type 'a t =
    { mutable size : int
    ; mutable data : 'a Option_array.t
    ; mutable frozen : bool
    }

  let create ?(size = 0) () =
    { size = 0
    ; data = Option_array.create ~len:(Int.round_up size ~to_multiple_of:2)
    ; frozen = false
    }
  ;;

  let freeze t =
    t.frozen <- true;
    t
  ;;

  let[@inline] check_not_frozen t = if t.frozen then invalid_arg "vec was frozen" else ()

  let copy t =
    let t' = create ~size:t.size () in
    Option_array.blit ~src:t.data ~dst:t'.data ~src_pos:0 ~dst_pos:0 ~len:t.size;
    t'
  ;;

  let[@inline] unsafe_get t i = A.unsafe_get_some_assuming_some t.data i

  let get t i =
    if i < 0 || i >= t.size then invalid_arg "index out of bounds" else unsafe_get t i
  ;;

  let get_opt t i = if i < 0 || i >= t.size then None else Some (unsafe_get t i)
  let last t = get_opt t (t.size - 1)
  let[@inline] unsafe_set t i x = A.unsafe_set_some t.data i x

  let set t i x =
    check_not_frozen t;
    if i < 0 || i >= t.size
    then raise (Invalid_argument "index out of bounds")
    else unsafe_set t i x
  ;;

  let[@inline] capacity t = Option_array.length t.data

  let shrink_to_fit t =
    check_not_frozen t;
    if t.size < capacity t
    then (
      let new_data = Option_array.create ~len:t.size in
      Option_array.blit ~src:t.data ~dst:new_data ~src_pos:0 ~dst_pos:0 ~len:t.size;
      t.data <- new_data)
  ;;

  let grow t ~desired =
    assert (desired > t.size);
    if desired > capacity t
    then (
      let new_cap = Int.round_up desired ~to_multiple_of:2 in
      let new_data = Option_array.create ~len:new_cap in
      Option_array.blit ~src:t.data ~dst:new_data ~src_pos:0 ~dst_pos:0 ~len:t.size;
      t.data <- new_data)
  ;;

  let[@inline] unsafe_push t x =
    if t.size = capacity t then grow t ~desired:(max 4 (t.size * 2));
    A.unsafe_set_some t.data t.size x;
    t.size <- t.size + 1
  ;;

  let push t x =
    check_not_frozen t;
    unsafe_push t x
  ;;

  let[@inline] unsafe_pop t =
    t.size <- t.size - 1;
    A.unsafe_get_some_assuming_some t.data t.size
  ;;

  let pop t =
    check_not_frozen t;
    if t.size = 0 then None else Some (unsafe_pop t)
  ;;

  let pop_exn t =
    check_not_frozen t;
    if t.size = 0 then invalid_arg "empty vec" else unsafe_pop t
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
    let t = create ~size:(Array.length a) () in
    Array.iteri a ~f:(fun i x -> unsafe_set t i x);
    t.size <- Array.length a;
    t
  ;;

  let of_list xs =
    let size = List.length xs in
    let t = create ~size () in
    List.iteri xs ~f:(fun i x -> unsafe_set t i x);
    t.size <- size;
    t
  ;;

  let to_list t = List.init t.size ~f:(unsafe_get t)

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

  let to_iter t f = iter t ~f
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

  let append_into ~into t =
    grow into ~desired:(into.size + t.size);
    Option_array.blit ~src:t.data ~dst:into.data ~src_pos:0 ~dst_pos:into.size ~len:t.size
  ;;

  let%test_unit _ =
    let v = create () in
    [%test_result: int] (length v) 0;
    let xs = [ 1; 2; 3; 4; 5 ] in
    List.iter xs ~f:(fun x -> push v x);
    [%test_result: int] (length v) (List.length xs);
    let xs_ref = ref [] in
    iter v ~f:(fun x -> xs_ref := x :: !xs_ref);
    [%test_result: int list] !xs_ref ~expect:(List.rev xs);
    [%test_result: int] (pop_exn v) ~expect:5;
    [%test_result: int] (pop_exn v) ~expect:4;
    ()
  ;;

  include Utils.Quickcheck.Make_quickcheck_list_conv (struct
      type nonrec 'a t = 'a t

      let of_list = of_list
      let to_list = to_list
    end)

  module C = Indexed_container.Make (struct
      type nonrec 'a t = 'a t

      let length = `Custom length
      let of_list = of_list
      let of_array = of_array
      let iter = `Custom iter
      let iteri = `Custom iteri
      let fold = fold
      let foldi = `Define_using_fold
    end)

  let%test_unit "of_list to_list" =
    Quickcheck.test
      [%quickcheck.generator: int list]
      ~sexp_of:[%sexp_of: int list]
      ~f:(fun xs ->
        let xs' = to_list @@ of_list xs in
        [%test_eq: int list] xs xs')
  ;;

  let%test_unit "push pop" =
    Quickcheck.test
      [%quickcheck.generator: int list]
      ~sexp_of:[%sexp_of: int list]
      ~f:(fun xs ->
        let vec = create () in
        List.iter xs ~f:(push vec);
        let xs' = to_list vec in
        [%test_eq: int list] xs xs';
        let xs'' = ref [] in
        let open F.Iter.Infix in
        0 -- (length vec - 1) |> F.Iter.iter ~f:(fun _ -> xs'' := pop_exn vec :: !xs'');
        [%test_eq: int list] xs !xs'')
  ;;
end

include Raw

type ('a, -'perms) t = 'a Raw.t

let equal f _ = Raw.equal f
let compare f _ = Raw.compare f
let hash_fold_t f _ = Raw.hash_fold_t f
let sexp_of_t f _ t = Raw.sexp_of_t f t
let t_of_sexp f _ sexp = Raw.t_of_sexp f sexp
let of_raw = Fn.id
let to_raw = Fn.id

let%expect_test _ =
  let r = Int.round_up 0 ~to_multiple_of:2 in
  print_s [%sexp (r : int)];
  [%expect {| 0 |}]
;;

let%expect_test _ =
  let t = of_list [ 1; 2; 3; 4; 5 ] in
  print_s [%sexp (t : (int, _) t)];
  [%expect {| (1 2 3 4 5) |}]
;;
