open O

type coordinate = { row : int; col : int } [@@deriving accessors, sexp]

type cell = { contents : string; location : coordinate }
[@@deriving accessors, sexp]

type shape = { cells : cell list } [@@deriving accessors, sexp]

let shift_shape_right =
  Accessor.map (cells @> Accessor.List.each @> location @> col) ~f:succ

let%expect_test _ =
  let shape =
    {
      cells =
        [
          { contents = "a"; location = { row = 0; col = 0 } };
          { contents = "b"; location = { row = 0; col = 1 } };
          { contents = "c"; location = { row = 1; col = 0 } };
          { contents = "d"; location = { row = 1; col = 1 } };
        ];
    }
  in
  printf "before\n";
  print_s [%sexp (shape : shape)];
  printf "after\n";
  print_s [%sexp (shift_shape_right shape : shape)];
  [%expect
    {|
    before
    ((cells
      (((contents a) (location ((row 0) (col 0))))
       ((contents b) (location ((row 0) (col 1))))
       ((contents c) (location ((row 1) (col 0))))
       ((contents d) (location ((row 1) (col 1)))))))
    after
    ((cells
      (((contents a) (location ((row 0) (col 1))))
       ((contents b) (location ((row 0) (col 2))))
       ((contents c) (location ((row 1) (col 1))))
       ((contents d) (location ((row 1) (col 2))))))) |}]

let row_and_col : (_, int, coordinate, [< many ]) Accessor.t =
  [%accessor
    Accessor.many (fun { row; col } ->
        let open Accessor.Many.Let_syntax in
        let%map_open row = access row and col = access col in
        { row; col })]

let%expect_test _ =
  let coord = { row = 1; col = 2 } in
  let res = A.map row_and_col coord ~f:succ in
  print_s [%sexp (res : coordinate)];
  [%expect {| ((row 2) (col 3)) |}]

module Reverse = struct
  module T = struct
    type t = string

    let sexp_of_t = String.sexp_of_t
    let t_of_sexp = String.t_of_sexp
    let compare x y = String.compare y x
  end

  include T
  include Comparator.Make (T)
end

let alist = [ ("foo", 0); ("snoo", 3) ]
let ord_map = Map.of_alist_exn (module String) alist
let rev_map = Map.of_alist_exn (module Reverse) alist

module App (M : sig end) : sig
  type t

  val zero : t
end = struct
  type t = int

  let zero = 0
end

(* A () argument signifies a generative functor in OCaml. *)
module Gen (M : sig end) () : sig
  type t

  val zero : t
end = struct
  type t = int

  let zero = 0
end

module Empty = struct end
module Empty' = struct end
module A = App (Empty)
module B = App (Empty)
module C = App (struct end) (* The argument is syntactically different. *)
module D = Gen (Empty) ()
module E = Gen (Empty) ()
module F = App (Empty')

(* let _ =
  (* A.t and B.t are compatible. *)
  ignore (A.zero == B.zero);

  (* OK *)

  (* A.t and C.t are not compatible because the functor arguments
   * are not syntactically equal. *)
  ignore (A.zero == C.zero);

  ignore (A.zero == F.zero);
  (* type error *)

  (* D.t and C.t are not compatible because they are produced
   * from generative functors. *)
  ignore (D.zero == E.zero)
(* type error *)
*)
