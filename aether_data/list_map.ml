open Core

type ('k, 'v) t =
  | T :
      { map : ('k, 'v, 'cmp) Map.t
      ; list : ('k * 'v) list
      }
      -> ('k, 'v) t

let empty cmp = T { map = Map.empty cmp; list = [] }
let of_alist_exn cmp list = T { map = Map.of_alist_exn cmp list; list }
let to_alist (T t) = t.list
let find (T t) = Map.find t.map
let find_exn (T t) = Map.find_exn t.map

let add (T { map; list }) ~key ~data =
  match Map.add map ~key ~data with
  | `Ok map -> `Ok (T { map; list = (key, data) :: list })
  | `Duplicate -> `Duplicate
;;

let add_exn (T { map; list }) ~key ~data =
  T { map = Map.add_exn map ~key ~data; list = (key, data) :: list }
;;

let compare f g (T t) (T t') =
  List.compare (Tuple2.compare ~cmp1:f ~cmp2:g) t.list t'.list
;;

let equal f g (T t) (T t') = List.equal (Tuple2.equal ~eq1:f ~eq2:g) t.list t'.list
let sexp_of_t f g (T t) = List.sexp_of_t (Tuple2.sexp_of_t f g) t.list

let%expect_test _ =
  let m = Map.of_alist_exn (module Int) [ 1, 2; 3, 4 ] in
  print_s [%sexp (m : int Int.Map.t)];
  [%expect {| ((1 2) (3 4)) |}]
;;
