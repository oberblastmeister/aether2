open! O
include Id_table_intf
module OA = Option_array

type ('k, 'v) t = { mutable a : ('k * 'v) Option_array.t }

let create ?(size = 0) () = { a = OA.create ~len:size }

let resize_array t size =
  let a = OA.create ~len:size in
  OA.blit ~src:t.a ~dst:a ~src_pos:0 ~dst_pos:0 ~len:(OA.length t.a);
  t.a <- a
;;

let length t = OA.length t.a

let resize_for_index t index =
  if index >= length t
  then (
    let new_size = max 4 (Int.round_up (index + 1) ~to_multiple_of:2) in
    resize_array t new_size)
;;

let find t k ~to_id =
  let i = Raw_id.to_int @@ to_id k in
  if i >= length t then None else Option.map ~f:snd @@ OA.get t.a @@ i
;;

let mem t k ~to_id =
  let i = Raw_id.to_int @@ to_id k in
  i < length t && OA.is_some t.a i
;;

let find_exn t k ~to_id =
  let i = Raw_id.to_int @@ to_id k in
  if i >= length t
  then raise_s [%message "key not found"]
  else snd @@ OA.get_some_exn t.a i
;;

let set t ~key:k ~data:v ~to_id =
  let index = Raw_id.to_int @@ to_id k in
  resize_for_index t index;
  OA.set_some t.a index (k, v)
;;

let foldi t ~init ~f =
  OA.fold t.a ~init ~f:(fun z -> Option.value_map ~default:z ~f:(fun x -> f z x))
;;

let iteri t = Container.iter ~fold:foldi t
let to_iteri t k = iteri t ~f:k

let fold t ~init ~f =
  OA.fold t.a ~init ~f:(fun z -> Option.value_map ~default:z ~f:(fun (_, v) -> f z v))
;;

let to_list t = Container.to_list ~fold:foldi t

let of_list l ~to_id =
  let t = create () in
  List.iter l ~f:(fun (k, v) -> set t ~key:k ~data:v ~to_id);
  t
;;

let sexp_of_t f g t = to_list t |> List.sexp_of_t (Tuple2.sexp_of_t f g)

module Make_gen (Arg : Gen_arg) = struct
  let create = create
  let find = find ~to_id:Arg.to_raw
  let find_exn = find_exn ~to_id:Arg.to_raw
  let set = set ~to_id:Arg.to_raw
  let mem = mem ~to_id:Arg.to_raw
end

module Make (Arg : Arg) = Make_gen (struct
    type ('a, 'b, 'c) t = Arg.t

    let to_raw = Arg.to_raw
  end)

let%test_module _ =
  (module struct
    module Sym = Name_id.Make ()
    module Table = Make (Sym)

    let set = String.Set.of_list
    let tbl = Hashtbl.create (module String)

    let sym s =
      match Hashtbl.find tbl s with
      | None ->
        let label = Sym.of_string_global_unique s in
        Hashtbl.set tbl ~key:s ~data:label;
        label
      | Some l -> l
    ;;

    let amount = 20

    let sym =
      let open F.Iter in
      F.Iter.int_range ~start:0 ~stop:amount
      |> F.Iter.map ~f:(fun i -> Int.to_string i |> sym)
      |> F.Iter.to_array
    ;;

    let%test_unit _ =
      let tab = create () in
      let check i res = [%test_result: int option] (Table.find tab sym.(i)) ~expect:res in
      F.Iter.int_range ~start:0 ~stop:amount |> F.Iter.iter ~f:(fun i -> check i None);
      Table.set tab ~key:sym.(2) ~data:2;
      check 2 (Some 2);
      check 1 None;
      check 10 None;
      Table.set tab ~key:sym.(18) ~data:1234;
      F.Iter.int_range ~start:10 ~stop:17 |> F.Iter.iter ~f:(fun i -> check i None);
      check 19 None;
      check 20 None;
      check 18 (Some 1234);
      ()
    ;;
  end)
;;
