open Core
module Vec = Vec.Raw
module M = Comparator
module F = Folds
include Array_heap_intf

type ('a, 'cmp) t = 'a Vec.t

let[@inline] left i = (i lsl 1) + 1 (* i * 2 + 1 = (i + 1) * 2 - 1 *)
let[@inline] right i = (i + 1) lsl 1 (* (i + 1) * 2 = (i + 1) * 2 + 1 - 1 *)
let[@inline] parent i = (i - 1) asr 1 (* (i-1) / 2 *)

module type Heap_ops = sig
  type ('k, 'v, 'cmp) t

  val unsafe_swap : ('k, 'v, 'cmp) t -> int -> int -> unit
  val unsafe_get : ('k, 'v, 'cmp) t -> int -> 'v
  val length : ('k, 'v, 'cmp) t -> int
end

module type Heapify = sig
  type ('k, 'v, 'cmp) t

  val heap_invariant : cmp:('v, 'cmp) Comparator.t -> ('k, 'v, 'cmp) t -> unit
  val unsafe_sink : cmp:('v, 'cmp) Comparator.t -> ('k, 'v, 'cmp) t -> int -> unit
  val unsafe_swim : cmp:('v, 'cmp) Comparator.t -> ('k, 'v, 'cmp) t -> int -> unit
end

module Make_heapify (H : Heap_ops) :
  Heapify with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) H.t = struct
  let[@inline] unsafe_gt ~cmp t i j =
    let x = H.unsafe_get t i in
    let y = H.unsafe_get t j in
    cmp.Comparator.compare x y > 0
  ;;

  let[@inline] unsafe_ge ~cmp t i j =
    let x = H.unsafe_get t i in
    let y = H.unsafe_get t j in
    cmp.Comparator.compare x y >= 0
  ;;

  let heap_invariant ~cmp t =
    let ge t i j =
      if i < H.length t && j < H.length t then unsafe_ge ~cmp t i j else true
    in
    let rec go i =
      if i < H.length t
      then (
        let l = left i in
        let r = right i in
        if not (ge t i l)
        then raise_s [%message "left child is greater than parent" (i : int) (l : int)];
        if not (ge t i r)
        then raise_s [%message "right child is greater than parent" (i : int) (r : int)];
        go l;
        go r)
    in
    go 0
  ;;

  let unsafe_sink ~cmp t i =
    let len = H.length t in
    let rec go i =
      let child = left i in
      (* if child in bounds, then i in bounds *)
      if child < len
      then (
        let child =
          if child + 1 < len && unsafe_gt ~cmp t (child + 1) child
          then child + 1
          else child
        in
        if unsafe_gt ~cmp t child i
        then (
          H.unsafe_swap t child i;
          go child))
    in
    go i
  ;;

  let unsafe_swim ~cmp t i =
    let rec go i =
      let p = parent i in
      if p >= 0 && unsafe_gt ~cmp t i p
      then (
        H.unsafe_swap t i p;
        go p)
    in
    go i
  ;;
end

let create ?size () = Vec.create ?size ()

include Make_heapify (struct
    type nonrec ('k, 'v, 'cmp) t = ('v, 'cmp) t

    let unsafe_swap = Vec.unsafe_swap
    let unsafe_get = Vec.unsafe_get
    let length = Vec.length
  end)

let push ~cmp t x =
  let i = Vec.length t in
  Vec.push t x;
  unsafe_swim ~cmp t i
;;

let unsafe_pop ~cmp t =
  let x = Vec.unsafe_get t 0 in
  Vec.unsafe_swap t 0 (Vec.length t - 1);
  unsafe_sink ~cmp t 0;
  x
;;

let get t i =
  if i < Vec.length t
  then Vec.unsafe_get t i
  else raise_s [%message "index out of bounds"]
;;

let length = Vec.length
let pop ~cmp t = if Vec.length t > 0 then Some (unsafe_pop ~cmp t) else None

module Indexed = struct
  module OA = Option_array
  module A = Array_ops.Option_array.Ops

  (* unstable index is the heap index *)
  type ('a, 'cmp) t =
    { mutable values : 'a Option_array.t (* stable index to values *)
    ; pq : int Vec.t
    ; mutable qp : int Option_array.t (* stable to unstable index *)
    ; sexp_of : 'a -> Sexp.t
    }

  let sexp_of_t f _ t =
    let pq = Vec.to_list t.pq |> List.map ~f:(fun i -> OA.get_some_exn t.values i, i) in
    Sexp.(
      List
        [ List [ Atom "values"; Option_array.sexp_of_t f t.values ]
        ; List [ Atom "pq"; List.sexp_of_t (Tuple2.sexp_of_t f Int.sexp_of_t) pq ]
        ; List [ Atom "qp"; Option_array.sexp_of_t Int.sexp_of_t t.qp ]
        ])
  ;;

  let validate_key key = if key < 0 then raise_s [%message "key out of bounds"]

  let[@inline] mem t key =
    key >= 0 && key < OA.length t.values && A.unsafe_is_some t.values key
  ;;

  let create ?(sexp_of = sexp_of_opaque) ?(size = 0) () =
    let len = size in
    { values = Option_array.create ~len
    ; pq = Vec.create ~size ()
    ; qp = Option_array.create ~len
    ; sexp_of
    }
  ;;

  let unsafe_get t i = A.unsafe_get_some_assuming_some t.values (Vec.unsafe_get t.pq i)

  let unsafe_swap t i j =
    Vec.unsafe_swap t.pq i j;
    A.unsafe_set_some t.qp (Vec.unsafe_get t.pq i) i;
    A.unsafe_set_some t.qp (Vec.unsafe_get t.pq j) j
  ;;

  let length t = Vec.length t.pq

  include Make_heapify (struct
      type nonrec ('k, 'v, 'cmp) t = ('v, 'cmp) t

      let unsafe_swap = unsafe_swap
      let unsafe_get = unsafe_get
      let length = length
    end)

  let invariant ~cmp t =
    [%test_eq: int] (OA.length t.values) (OA.length t.qp);
    F.Iter.(
      0 -- (OA.length t.values - 1)
      |> filter ~f:(fun i -> OA.is_some t.values i)
      |> iter ~f:(fun key ->
        if not @@ OA.is_some t.qp key
        then raise_s [%message "qp not some when values was some" (key : int)];
        let heap_index = OA.get_some_exn t.qp key in
        let heap_len = Vec.length t.pq in
        if not (heap_index >= 0 && heap_index < Vec.length t.pq)
        then
          raise_s
            [%message
              "heap index was not in bounds"
                (key : int)
                (heap_index : int)
                (heap_len : int)];
        let stable_index = Vec.get t.pq heap_index in
        if not (stable_index = key)
        then
          raise_s
            [%message
              "heap index to stable index was not correct"
                (key : int)
                (heap_index : int)
                (stable_index : int)]));
    heap_invariant ~cmp t
  ;;

  let unsafe_modify ~cmp t ~key ~f =
    let heap_index = A.unsafe_get_some_assuming_some t.qp key in
    let value = A.unsafe_get_some_assuming_some t.values key in
    let new_value = f value in
    match cmp.Comparator.compare new_value value |> Ordering.of_int with
    | Ordering.Less ->
      OA.set_some t.values key new_value;
      unsafe_sink ~cmp t heap_index
    | Ordering.Equal -> ()
    | Ordering.Greater ->
      OA.set_some t.values key new_value;
      unsafe_swim ~cmp t heap_index
  ;;

  let modify ~cmp t ~key ~f =
    validate_key key;
    if mem t key then unsafe_modify ~cmp t ~key ~f
  ;;

  let set ~cmp t ~key ~data =
    validate_key key;
    if mem t key
    then unsafe_modify ~cmp t ~key ~f:(fun _ -> data)
    else (
      if key >= OA.length t.values
      then (
        t.values <- Utils.Option_array.resize_for_index t.values key;
        t.qp <- Utils.Option_array.resize_for_index t.qp key);
      A.unsafe_set_some t.values key data;
      let heap_len = Vec.length t.pq in
      A.unsafe_set_some t.qp key heap_len;
      Vec.unsafe_push t.pq key;
      unsafe_swim ~cmp t heap_len)
  ;;

  let unsafe_remove ~cmp t key =
    let heap_index = A.unsafe_get_some_assuming_some t.qp key in
    let heap_len = length t in
    (* print_s [%message "removing" (key : int) (heap_index : int)]; *)
    let v = A.unsafe_get_some_assuming_some t.values key in
    unsafe_swap t heap_index (heap_len - 1);
    A.unsafe_set_none t.values key;
    A.unsafe_set_none t.qp key;
    let _ = Vec.unsafe_pop t.pq in
    (* if it was the last index, we don't have to do anything because we just popped it above *)
    if heap_index <> heap_len - 1
    then (
      unsafe_swim ~cmp t heap_index;
      unsafe_sink ~cmp t heap_index);
    v
  ;;

  let remove ~cmp t i =
    if i < 0 || i >= OA.length t.values then raise_s [%message "index out of bounds"];
    if OA.is_some t.qp i then Some (unsafe_remove ~cmp t i) else None
  ;;

  let pop ~cmp t =
    if length t = 0 then None else Some (unsafe_remove ~cmp t (Vec.unsafe_get t.pq 0))
  ;;

  module Make (C : Key) = struct
    open struct
      let cmp = C.comparator
    end

    type nonrec t = (C.t, C.comparator_witness) t

    let sexp_of_t t = sexp_of_t C.sexp_of_t sexp_of_opaque t
    let create ?size () = create ~sexp_of:C.sexp_of_t ?size ()
    let invariant = invariant ~cmp
    let pop = pop ~cmp
    let remove = remove ~cmp
    let set = set ~cmp
    let modify = modify ~cmp
  end

  let%test_unit _ =
    let module Heap = Make (Int) in
    let n = 1000 in
    let ipq = create () in
    Heap.invariant ipq;
    let is = F.Iter.(0 -- n |> filter ~f:(fun i -> i mod 3 = 0)) in
    F.Iter.(
      is
      |> iter ~f:(fun i ->
        Heap.set ipq ~key:i ~data:i;
        Heap.invariant ipq));
    F.Iter.(
      is
      |> rev
      |> iter ~f:(fun i ->
        let data = Heap.pop ipq |> Option.value_exn in
        Heap.invariant ipq;
        [%test_eq: int] data i));
    [%test_eq: int] (length ipq) 0
  ;;

  let%test_unit _ =
    let module Heap = Make (Int) in
    let n = 1000 in
    let ipq = create () in
    Heap.set ipq ~key:(n + 1) ~data:(n + 1);
    Heap.invariant ipq;
    F.Iter.(
      0 -- n
      |> iter ~f:(fun i ->
        Heap.set ipq ~key:i ~data:i;
        Heap.invariant ipq));
    F.Iter.(
      0 -- n
      |> filter ~f:(fun i -> i mod 3 = 1 || i mod 3 = 2)
      |> iter ~f:(fun i ->
        (* print_s [%message "removing from heap" (ipq : Heap.t) (i : int)]; *)
        let data = Heap.remove ipq i |> Option.value_exn in
        Heap.invariant ipq;
        [%test_eq: int] data i));
    Heap.set ipq ~key:(n + 1) ~data:(-1);
    F.Iter.(
      0 -- n
      |> filter ~f:(fun i -> i mod 3 = 0)
      |> rev
      |> iter ~f:(fun i ->
        let data = Heap.pop ipq |> Option.value_exn in
        [%test_eq: int] data i));
    [%test_eq: int] (length ipq) 1;
    [%test_result: int] (Heap.pop ipq |> Option.value_exn) ~expect:(-1);
    Heap.invariant ipq;
    [%test_eq: int] (length ipq) 0
  ;;

  let%test_unit _ =
    let module Heap = Make (Int) in
    let ipq = create () in
    Heap.invariant ipq;
    Heap.set ipq ~key:3 ~data:12;
    Heap.invariant ipq;
    Heap.set ipq ~key:4 ~data:10;
    Heap.invariant ipq;
    Heap.set ipq ~key:10 ~data:13;
    Heap.invariant ipq;
    Heap.set ipq ~key:1 ~data:1;
    Heap.invariant ipq;
    Heap.set ipq ~key:7 ~data:2;
    Heap.invariant ipq;
    [%test_result: int] (Heap.pop ipq |> Option.value_exn) ~expect:13;
    Heap.set ipq ~key:1 ~data:40;
    Heap.invariant ipq;
    [%test_result: int] (Heap.pop ipq |> Option.value_exn) ~expect:40;
    Heap.invariant ipq;
    [%test_result: int] (Heap.pop ipq |> Option.value_exn) ~expect:12;
    Heap.invariant ipq;
    [%test_result: int] (Heap.pop ipq |> Option.value_exn) ~expect:10;
    Heap.invariant ipq;
    ()
  ;;
end
