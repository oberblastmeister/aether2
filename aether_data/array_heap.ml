open Core
module Vec = Vec.Raw
module M = Comparator

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

module Ops = struct
  type nonrec ('k, 'v, 'cmp) t = ('v, 'cmp) t

  let unsafe_swap = Vec.unsafe_swap
  let unsafe_get = Vec.unsafe_get
  let length = Vec.length
end

module type Heapify = sig
  type ('k, 'v, 'cmp) t

  val unsafe_sink : cmp:('v, 'cmp) Comparator.t -> ('k, 'v, 'cmp) t -> int -> unit
  val unsafe_swim : cmp:('v, 'cmp) Comparator.t -> ('k, 'v, 'cmp) t -> int -> unit
end

module Make_heapify (H : Heap_ops) :
  Heapify with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) H.t = struct
  let[@inline] gt ~cmp t i j =
    let x = H.unsafe_get t i in
    let y = H.unsafe_get t j in
    cmp.Comparator.compare x y > 0
  ;;

  let unsafe_sink ~cmp t i =
    let len = H.length t in
    let rec go i =
      let child = left i in
      (* if child in bounds, then i in bounds *)
      if child < len
      then (
        let child =
          if child + 1 < len && gt ~cmp t (child + 1) child then child + 1 else child
        in
        if gt ~cmp t child i
        then (
          H.unsafe_swap t child i;
          go child))
    in
    go i
  ;;

  let unsafe_swim ~cmp t i =
    let rec go i =
      let p = parent i in
      if p >= 0 && gt ~cmp t i p
      then (
        H.unsafe_swap t i p;
        print_s [%message "going up"];
        go p)
    in
    go i
  ;;
end

let create ?size () = Vec.create ?size ()

include Make_heapify (Ops)

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
  (* unstable index is the heap index *)
  type ('a, 'cmp) t =
    { values : 'a Vec.t (* stable index to values *)
    ; pq : int Vec.t (* unstable index to stable index *)
    ; qp : int Vec.t (* stable to unstable index *)
    }

  let create ?size () =
    { values = Vec.create ?size (); pq = Vec.create ?size (); qp = Vec.create ?size () }
  ;;

  let length t = Vec.length t.pq
  let unsafe_get t i = Vec.unsafe_get t.values (Vec.unsafe_get t.pq i)

  let unsafe_swap t i j =
    Vec.unsafe_swap t.pq i j;
    Vec.unsafe_set t.qp (Vec.unsafe_get t.pq i) i;
    Vec.unsafe_set t.qp (Vec.unsafe_get t.pq j) j
  ;;

  include Make_heapify (struct
      type nonrec ('k, 'v, 'cmp) t = ('v, 'cmp) t

      let unsafe_swap = unsafe_swap
      let unsafe_get = unsafe_get
      let length = length
    end)

  let unsafe_pop ~cmp t =
    let stable_index = Vec.unsafe_get t.pq 0 in
    let v = Vec.unsafe_get t.values 0 in
    unsafe_swap t 0 (length t - 1);
    unsafe_sink ~cmp t 0;
    Vec.unsafe_set t.qp stable_index (-1);
    let _ = Vec.unsafe_pop t.pq in
    v
  ;;

  let pop ~cmp t =
    if length t > 0
    then (
      let x = unsafe_pop ~cmp t in
      Some x)
    else None
  ;;

  let push ~cmp t v =
    let n = length t in
    Vec.push t.qp n;
    Vec.push t.pq n;
    Vec.push t.values v;
    unsafe_swim ~cmp t n
  ;;

  let remove ~cmp t i =
    if i < 0 || i >= length t then raise_s [%message "index out of bounds"];
    let heap_index = Vec.unsafe_get t.qp i in
    unsafe_swap t heap_index (length t - 1);
    let _ = Vec.unsafe_pop t.pq in
    unsafe_swim ~cmp t heap_index;
    unsafe_sink ~cmp t heap_index;
    Vec.unsafe_set t.qp i (-1)
  ;;

  module Make_with_comparator (C : Comparator.S) = struct
    open struct
      let cmp = C.comparator
    end

    type nonrec t = (C.t, C.comparator_witness) t

    let pop = pop ~cmp
    let push = push ~cmp
    let remove = remove ~cmp
  end

  let%test_unit _ =
    let module Heap = Make_with_comparator (Int) in
    let ipq = create () in
    Heap.push ipq 12;
    Heap.push ipq 10;
    Heap.push ipq 13;
    Heap.push ipq 1;
    Heap.push ipq 2;
    [%test_result: int] (Heap.pop ipq |> Option.value_exn) ~expect:13;
    ()
  ;;
end
