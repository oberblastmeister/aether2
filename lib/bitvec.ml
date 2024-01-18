open! O

let width_ = 8

(* Helper functions *)
let[@inline] get_ b i = Char.to_int (Bytes.get b i)
let[@inline] unsafe_get_ b i = Char.to_int (Bytes.unsafe_get b i)
let[@inline] unsafe_set_ b i v = Bytes.unsafe_set b i (Char.unsafe_of_int v)
let[@inline] mod_ n = n land 0b111
let[@inline] div_ n = n lsr 3
let[@inline] mul_ n = n lsl 3
let zero = Char.unsafe_of_int 0

(* 0b11111111 *)
let all_ones_ = Char.unsafe_of_int ((1 lsl width_) - 1)
let () = assert (Char.equal all_ones_ (Char.of_int_exn 0b1111_1111))

(* [lsb_mask_ n] is [0b111111] with [n] ones. *)
let[@inline] lsb_mask_ n = (1 lsl n) - 1

type t =
  { mutable b : bytes
  ; mutable size : int
  }

let[@inline] length t = t.size
let empty () = { b = Bytes.create 0; size = 0 }
let bytes_length_of_size size = if mod_ size = 0 then div_ size else div_ size + 1

let create ~size default =
  if size = 0
  then empty ()
  else (
    let n = bytes_length_of_size size in
    let b = if default then Bytes.make n all_ones_ else Bytes.make n zero in
    (* adjust last bits *)
    let r = mod_ size in
    if default && r <> 0 then unsafe_set_ b (n - 1) (lsb_mask_ r);
    { b; size })
;;

let copy bv = { bv with b = Bytes.sub bv.b ~pos:0 ~len:(bytes_length_of_size bv.size) }
let[@inline] capacity bv = mul_ (Bytes.length bv.b)

let really_resize_ bv ~desired ~current size =
  bv.size <- size;
  if desired <> current
  then (
    let b = Bytes.make desired zero in
    Bytes.blit ~src:bv.b ~src_pos:0 ~dst:b ~dst_pos:0 ~len:(min desired current);
    bv.b <- b)
;;

let[@inline never] grow_to_at_least_real_ bv size =
  (* beyond capacity *)
  let current = Bytes.length bv.b in
  let desired = bytes_length_of_size size in
  let desired = min Sys.max_string_length (max desired (current + (current / 2))) in
  assert (desired > current);
  really_resize_ bv ~desired ~current size
;;

let grow_to_at_least_ bv size =
  if size <= capacity bv
  then (* within capacity *)
    bv.size <- size
  else
    (* resize. This is a separate function so it's easier to
       inline the happy path. *)
    grow_to_at_least_real_ bv size
;;

let[@inline never] clear_bits_above_ bv top =
  let n = div_ top in
  let j = mod_ top in
  Bytes.fill
    bv.b
    ~pos:(n + 1)
    ~len:(bytes_length_of_size bv.size - n - 1)
    (Char.unsafe_of_int 0);
  unsafe_set_ bv.b n (unsafe_get_ bv.b n land lsb_mask_ j)
;;

let shrink_ bv size =
  assert (size <= bv.size);
  if size < bv.size
  then (
    let desired = bytes_length_of_size size in
    let current = Bytes.length bv.b in
    clear_bits_above_ bv size;
    really_resize_ bv ~desired ~current size)
;;

(* let resize bv size =
   if size < 0 then invalid_arg "resize: negative size";
   if size < bv.size then (
   clear_bits_above_ bv size;
   bv.size <- size
   ) else if size > bv.size then
   grow_to_at_least_ bv size *)

let[@inline] get bv i =
  if i < 0 then invalid_arg "get: negative index";
  let idx_bucket = div_ i in
  let idx_in_byte = mod_ i in
  if idx_bucket < Bytes.length bv.b
  then unsafe_get_ bv.b idx_bucket land (1 lsl idx_in_byte) <> 0
  else false
;;

let[@inline] set bv i =
  if i < 0
  then invalid_arg "set: negative index"
  else (
    let idx_bucket = div_ i in
    let idx_in_byte = mod_ i in
    if i >= bv.size then grow_to_at_least_ bv (i + 1);
    unsafe_set_ bv.b idx_bucket (unsafe_get_ bv.b idx_bucket lor (1 lsl idx_in_byte)))
;;

let[@inline] unset bv i =
  if i < 0
  then invalid_arg "reset: negative index"
  else (
    let n = div_ i in
    let j = mod_ i in
    if i >= bv.size then grow_to_at_least_ bv (i + 1);
    unsafe_set_ bv.b n (unsafe_get_ bv.b n land lnot (1 lsl j)))
;;

let[@inline] set_bool bv i b = if b then set bv i else unset bv i

let union_into_no_resize_ ~into bv =
  assert (Bytes.length into.b >= bytes_length_of_size bv.size);
  for i = 0 to bytes_length_of_size bv.size - 1 do
    unsafe_set_ into.b i (unsafe_get_ into.b i lor unsafe_get_ bv.b i)
  done
;;

(* Underlying size grows for union. *)
let union_into ~into bv =
  if into.size < bv.size then grow_to_at_least_ into bv.size;
  union_into_no_resize_ ~into bv
;;

(* To avoid potentially 2 passes, figure out what we need to copy. *)
let union b1 b2 =
  if b1.size <= b2.size
  then (
    let into = copy b2 in
    union_into_no_resize_ ~into b1;
    into)
  else (
    let into = copy b1 in
    union_into_no_resize_ ~into b2;
    into)
;;

let inter_into_no_resize_ ~into bv =
  assert (into.size <= bv.size);
  for i = 0 to bytes_length_of_size into.size - 1 do
    unsafe_set_ into.b i (unsafe_get_ into.b i land unsafe_get_ bv.b i)
  done
;;

(* Underlying size shrinks for inter. *)
let inter_into ~into bv =
  if into.size > bv.size then shrink_ into bv.size;
  inter_into_no_resize_ ~into bv
;;

let inter b1 b2 =
  if b1.size <= b2.size
  then (
    let into = copy b1 in
    inter_into_no_resize_ ~into b2;
    into)
  else (
    let into = copy b2 in
    inter_into_no_resize_ ~into b1;
    into)
;;

(* Underlying size depends on the [in_] set for diff, so we don't change
   its size! *)
let diff_into ~into bv =
  let n = min (Bytes.length into.b) (Bytes.length bv.b) in
  for i = 0 to n - 1 do
    unsafe_set_ into.b i (unsafe_get_ into.b i land lnot (unsafe_get_ bv.b i))
  done
;;

let diff in_ not_in =
  let into = copy in_ in
  diff_into ~into not_in;
  into
;;

let iter_bytes_ (b : t) ~f =
  for n = 0 to div_ b.size - 1 do
    f (mul_ n) width_ (unsafe_get_ b.b n)
  done;
  let r = mod_ b.size in
  if r <> 0
  then (
    let last = div_ b.size in
    f (mul_ last) r (lsb_mask_ r land unsafe_get_ b.b last))
;;

let iteri bv ~f =
  iter_bytes_ bv ~f:(fun off width_n word_n ->
    for i = 0 to width_n - 1 do
      f (off + i) (word_n land (1 lsl i) <> 0)
    done)
;;

let iter bv ~f = iteri bv ~f:(fun _ b -> f b)
let to_iter bv k = iter bv ~f:k

let init size ~f : t =
  let v = create ~size false in
  for i = 0 to size - 1 do
    if f i then set v i
  done;
  v
;;

(* let concat bvs = *)
(* Interpret these as indices. *)
(* let of_list l =
   let size =
   match l with
   | [] -> 0
   | _ -> List.fold_left ~f:max ~init:0 l + 1
   in
   let bv = create ~size false in
   List.iter ~f:(fun i -> set bv i) l;
   bv
   ;; *)

(* let to_list bv =
   let l = ref [] in
   iteri bv ~f:(fun i b -> if b then l := i :: !l);
   !l
   ;; *)

let of_array a =
  let bv = create ~size:(Array.length a) false in
  Array.iteri ~f:(fun i b -> if b then set bv i) a;
  bv
;;

let to_array bv =
  let a = Array.create ~len:bv.size false in
  iteri bv ~f:(fun i b -> a.(i) <- b);
  a
;;

let to_list bv =
  let l = ref [] in
  iter bv ~f:(fun b -> l := b :: !l);
  List.rev !l
;;

let of_list l =
  let bv = create ~size:(List.length l) false in
  List.iteri ~f:(set_bool bv) l;
  bv
;;

(* naive implementation *)
let concat bvs =
  let size = List.fold_left ~f:(fun s bv -> s + length bv) ~init:0 bvs in
  let bv = create ~size false in
  let i = ref 0 in
  List.iter bvs ~f:(fun bv ->
    iter bv ~f:(fun b ->
      set_bool bv !i b;
      incr i));
  bv
;;

let fold bv ~init ~f = to_iter bv |> F.Iter.fold ~init ~f

module C = struct
  type elt = bool

  include Indexed_container.Make0_with_creators (struct
      module Elt = Bool

      type nonrec t = t

      let length = `Custom length
      let of_list = of_list
      let of_array = of_array
      let concat = concat
      let iter = `Custom iter
      let iteri = `Custom iteri
      let init = `Custom init
      let concat_mapi = `Define_using_concat
      let fold = fold
      let foldi = `Define_using_fold
    end)
end

include Utils.Make_quickcheck_list_conv0 (struct
    type nonrec t = t
    type elt = bool [@@deriving quickcheck]

    let of_list = of_list
    let to_list = to_list
  end)

let equal_bytes_ size b1 b2 =
  try
    for i = 0 to bytes_length_of_size size - 1 do
      if not @@ Char.equal (Bytes.get b1 i) (Bytes.get b2 i) then raise_notrace Exit
    done;
    true
  with
  | Exit -> false
;;

let equal x y : bool = x.size = y.size && equal_bytes_ x.size x.b y.b
let sexp_of_t bv = to_array bv |> [%sexp_of: bool array]
let t_of_sexp sexp = [%of_sexp: bool array] sexp |> of_array
let qtest f = Quickcheck.test [%quickcheck.generator: t] ~sexp_of:[%sexp_of: t] ~f

let%test_unit _ =
  qtest (fun bv ->
    let bv' = of_list (to_list bv) in
    assert (equal bv bv'))
;;

let%test_unit _ =
  qtest (fun bv ->
    let bv' = of_array (to_array bv) in
    assert (equal bv bv'))
;;

let ix_test f =
  Quickcheck.test
    (List.quickcheck_generator (Int.gen_incl 0 1000))
    ~sexp_of:[%sexp_of: int list]
    ~f
;;

let%test_unit "set unset" =
  Quickcheck.test
    (Quickcheck.Generator.tuple2 [%quickcheck.generator: t] (Int.gen_incl 0 100))
    ~sexp_of:[%sexp_of: t * int]
    ~f:(fun (bv, i) ->
      set bv i;
      unset bv i;
      [%test_eq: bool] false (get bv i))
;;

let%test_unit _ = ix_test (fun ixs -> ())
