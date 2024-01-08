open Core
open Core
open Core_bench
open Folds.O
module F = Folds

module These = struct
  type ('a, 'b) t =
    | This of 'a
    | That of 'b
    | These of ('a * 'b)
    | Those of ('a * 'b * 'a * 'b)

  let to_list : ('a, 'a) t -> 'a list = function
    | This x -> [ x ]
    | That x -> [ x ]
    | These (x, y) -> [ x; y ]
    | Those (a, b, c, d) -> [ a; b; c; d ]
  ;;

  let fold t ~init ~f =
    match t with
    | This x -> f init x
    | That x -> f init x
    | These (x, y) -> f (f init x) y
    | Those (a, b, c, d) -> f (f (f (f init a) b) c) d
  ;;

  let folder t k = fold t ~init:() ~f:(fun () x -> k x)
end

module MyRecord = struct
  type t =
    { first : int
    ; second : int
    ; third : int
    }
  [@@deriving fields]
end

module MyRecordA = struct
  type t =
    { first : int
    ; second : int
    ; third : int
    }
end

let insert_list xs create set find =
  let q = Quickcheck.random_value String.gen_nonempty in
  let t = create () in
  List.iter xs ~f:(fun x -> set t ~key:q ~data:x);
  ()
;;

let main () =
  Random.self_init ();
  let strings =
    List.gen_with_length 1000 (String.gen_with_length 20 Char.gen_alphanum)
    |> Quickcheck.random_value
  in
  let xs =
    Sequence.repeat (These.Those (1234, 1324, 1234, 12341324))
    |> Fn.flip Sequence.take 10000
    |> Sequence.to_list
  in
  let r : MyRecord.t = { first = 1234; second = 1; third = 12 } in
  let rs =
    Sequence.repeat (These.Those (r, r, r, r))
    |> Fn.flip Sequence.take 10000
    |> Sequence.to_list
  in
  let ys =
    Sequence.repeat ({ first = 1234; second = 1324; third = 12 } : MyRecord.t)
    |> Fn.flip Sequence.take 10000
    |> Sequence.to_list
  in
  let zs =
    Sequence.repeat ({ first = 1234; second = 1324; third = 12 } : MyRecordA.t)
    |> Fn.flip Sequence.take 10000
    |> Sequence.to_list
  in
  let ls =
    Sequence.repeat [ 1, 2; 3, 4; 5, 6; 7, 8; 9, 10; 11, 12 ]
    |> Fn.flip Sequence.take 10000
    |> Sequence.to_list
  in
  Bench.make_command
    [ Bench.Test.create ~name:"fold to list" (fun () ->
        let res =
          F.Fold.reduce
            (F.Core.List.fold @> F.Core.List.fold @> F.Fold.of_fn fst)
            F.Reduce.to_list_rev
            ls
        in
        ())
    ; Bench.Test.create ~name:"normal to list" (fun () ->
        let res = List.concat_map ~f:(fun l -> List.map l ~f:(fun (x, y) -> x)) ls in
        ())
    ; Bench.Test.create ~name:"sequence to list" (fun () ->
        let res =
          Sequence.of_list ls
          |> Sequence.concat_map ~f:(fun l -> Sequence.of_list l |> Sequence.map ~f:fst)
          |> Sequence.to_list
        in
        ())
    ; Bench.Test.create ~name:"double map combinators" (fun () ->
        let res = (List.map & List.map & Tuple2.map) ~f:(fun x -> x + 1) ls in
        ())
    ; Bench.Test.create ~name:"fold sum" (fun () ->
        let res =
          List.fold ~f:(fun z x -> These.fold ~f:(fun z x -> x + z) ~init:z x) xs ~init:0
        in
        ())
    ; Bench.Test.create ~name:"fold sum with fields" (fun () ->
        let res =
          F.Fold.reduce
            (F.Core.List.fold @> These.folder @> F.Fold.of_fn MyRecord.first)
            F.Reduce.sum
            rs
        in
        ())
    ; Bench.Test.create ~name:"combinator fold sum" (fun () ->
        let res = F.Fold.reduce (F.Core.List.fold @> These.folder) F.Reduce.sum xs in
        ())
    ; Bench.Test.create ~name:"fold mut sum" (fun () ->
        let i = ref 0 in
        let res =
          List.fold
            ~f:(fun z x -> These.fold ~f:(fun _ x -> i := !i + 1) ~init:z x)
            xs
            ~init:()
        in
        let res = !i in
        ())
    ]
  |> Command_unix.run
;;

let () = main ()
