include Map_intf
open! Core
module OA = Option_array
module F = Folds

type ('k, 'v) t =
  { mutable a : ('k * 'v) Option_array.t
  ; sexp_of_key : 'k -> Sexp.t
  }

let create ?(sexp_of_key = sexp_of_opaque) ?(size = 0) () =
  { a = OA.create ~len:size; sexp_of_key }
;;

let size t = OA.length t.a

let resize_for_index t index =
  if index >= size t then t.a <- Aether_data.Utils.Option_array.resize_for_index t.a index
;;

let find t k ~to_int =
  let i = to_int k in
  if i >= size t then None else Option.map ~f:snd @@ OA.get t.a @@ i
;;

let mem t k ~to_int =
  let i = to_int k in
  i < size t && OA.is_some t.a i
;;

let key_not_found t k = raise_s [%message "key not found" ~key:(t.sexp_of_key k : Sexp.t)]

let find_exn t k ~to_int =
  let i = to_int k in
  if i >= size t
  then key_not_found t k
  else if OA.is_none t.a i
  then key_not_found t k
  else snd @@ OA.get_some_exn t.a i
;;

let set t ~key:k ~data:v ~to_int =
  let index = to_int k in
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

let of_list l ~to_int =
  let t = create () in
  List.iter l ~f:(fun (k, v) -> set t ~key:k ~data:v ~to_int);
  t
;;

let of_iter ?sexp_of_key ?size i ~to_int =
  let t = create ?sexp_of_key ?size () in
  F.Iter.iter i ~f:(fun (k, v) ->
    (* if mem ~to_int t k then raise_s [%message "key was present twice"]; *)
    set t ~key:k ~data:v ~to_int);
  t
;;

let sexp_of_t f g t = to_list t |> List.sexp_of_t (Tuple2.sexp_of_t f g)
let update t k ~to_int ~f = set t ~key:k ~data:(f (find t k ~to_int)) ~to_int

module Make_gen (Arg : Gen_arg) = struct
  open struct
    let to_int = Arg.to_int
  end

  let create ?size () = create ?size ()
  let find = find ~to_int
  let find_exn = find_exn ~to_int
  let set = set ~to_int
  let mem = mem ~to_int
  let update = update ~to_int
  let of_list = of_list ~to_int
  let of_iter ?size i = of_iter ~sexp_of_key:sexp_of_opaque ?size ~to_int i
  let ( .![] ) = find_exn
  let ( .?[] ) = find
  let ( .![]<- ) t key data = set t ~key ~data
end

module Make (Arg : Arg) = struct
  type k = Arg.t
  type nonrec 'v t = (k, 'v) t

  let create_real ?size () = create ~sexp_of_key:Arg.sexp_of_t ?size ()
  let of_iter_real ?size = of_iter ~sexp_of_key:Arg.sexp_of_t ?size ~to_int:Arg.to_int

  include Make_gen (struct
      type ('a, 'b, 'c) t = Arg.t

      let to_int = Arg.to_int
    end)

  let sexp_of_t f = sexp_of_t Arg.sexp_of_t f
  let create = create_real
  let of_iter = of_iter_real
end

let%test_module _ =
  (module struct
    module Sym = Name.Make ()
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
