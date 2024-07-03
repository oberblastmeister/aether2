open O

module Move = struct
  type ('a, 'o) t =
    { dst : 'o
    ; src : 'o
    ; ann : 'a
    }
  [@@deriving sexp_of]

  let create ~dst ~src ~ann = { dst; src; ann }
end

(* state of each edge being considered in the algorithm *)
type status =
  | To_move
  | Being_moved
  | Moved
[@@deriving equal]

(* https://xavierleroy.org/publi/parallel-move.pdf *)
(* key: every component has one cycle, because every destination is unique in the parallel move *)
(* this means that we only need one temp because we know it won't be overwritten by another cycle *)
let convert ~eq ~scratch (par_move : _ Move.t list) =
  let par_move = Array.of_list par_move in
  let n = Array.length par_move in
  let status = Array.init n ~f:(Fn.const To_move) in
  let sequential = Vec.create () in
  let did_use_scratch = ref false in
  let rec move_one i =
    (* self moves don't do anything, so skip them *)
    if not (eq par_move.(i).src par_move.(i).dst)
    then (
      (* if we see Being_moved in the children then we found the unique cycle *)
      status.(i) <- Being_moved;
      (* visit children *)
      for j = 0 to n - 1 do
        (* found a child move whose source will be overwritten by the current move's destination *)
        if eq par_move.(j).src par_move.(i).dst
        then (
          match status.(j) with
          | To_move -> move_one j
          | Being_moved ->
            (* unique cycle! *)
            did_use_scratch := true;
            let t = scratch par_move.(j).src in
            (* Vec.push sequential @@ move ~dst:t ~src:par_move.(j).src; *)
            Vec.push sequential { Move.dst = t; src = par_move.(j).src; ann = par_move.(j).ann };
            (* j now should move from the temp because we are about to overwrite j below *)
            par_move.(j) <- { (par_move.(j)) with src = t }
          | Moved -> ());
        ()
      done;
      (* move ourselves after all the children have been moved *)
      (* Vec.push sequential @@ move ~dst:par_move.(i).dst ~src:par_move.(i).src; *)
      Vec.push sequential @@ par_move.(i);
      status.(i) <- Moved;
      ())
  in
  (* make sure all components are traversed *)
  for i = 0 to n - 1 do
    if equal_status status.(i) To_move then move_one i
  done;
  sequential |> Vec.to_list, !did_use_scratch
;;

let%test_module _ =
  (module struct
    let rsi = "rsi"
    let rdi = "rdi"
    let rdx = "rdx"
    let rcx = "rcx"
    let rax = "rax"
    let rbx = "rbx"
    let r8 = "r8"
    let r9 = "r9"
    let r10 = "r10"
    let r11 = "r11"
    let a = "a"
    let b = "b"
    let c = "c"
    let d = "d"
    let scratch = "scratch"
    let convert par_move = convert ~eq:String.equal ~scratch:(Fn.const scratch) par_move

    let pmov dsts srcs =
      List.zip_exn dsts srcs |> List.map ~f:(fun (dst, src) -> Move.create ~dst ~src ~ann:())
    ;;

    let%expect_test "simple no scratch" =
      let res, did_use_scratch = convert (pmov [ b; d; c ] [ a; a; b ]) in
      [%test_result: bool] did_use_scratch ~expect:false;
      print_s [%sexp (res : (unit, string) Move.t list)];
      ();
      [%expect {|
        (((dst c) (src b) (ann ())) ((dst b) (src a) (ann ()))
         ((dst d) (src a) (ann ()))) |}]
    ;;

    let%expect_test "simple scratch" =
      let res, did_use_scratch = convert (pmov [ b; d; c; a ] [ a; a; b; c ]) in
      [%test_result: bool] did_use_scratch ~expect:true;
      print_s [%sexp (res : (unit, string) Move.t list)];
      ();
      [%expect
        {|
        (((dst scratch) (src a) (ann ())) ((dst d) (src a) (ann ()))
         ((dst a) (src c) (ann ())) ((dst c) (src b) (ann ()))
         ((dst b) (src scratch) (ann ()))) |}]
    ;;

    let%expect_test "multiple components" =
      let res, did_use_scratch =
        convert (pmov [ rdi; rsi; rdx; rcx; r8; r9 ] [ rsi; rdi; rsi; rsi; r9; r8 ])
      in
      [%test_result: bool] did_use_scratch ~expect:true;
      print_s [%sexp (res : (unit, string) Move.t list)];
      ();
      [%expect
        {|
        (((dst scratch) (src rsi) (ann ())) ((dst rdx) (src rsi) (ann ()))
         ((dst rcx) (src rsi) (ann ())) ((dst rsi) (src rdi) (ann ()))
         ((dst rdi) (src scratch) (ann ())) ((dst scratch) (src r9) (ann ()))
         ((dst r9) (src r8) (ann ())) ((dst r8) (src scratch) (ann ()))) |}]
    ;;
  end)
;;
