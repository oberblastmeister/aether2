type t =
  (* equal*)
  | E
  (* not equal*)
  | NE
  (* below (unsigned <) *)
  | B
  (* below or equal *)
  | BE
  (* above (unsigned >) *)
  | A
  (* above or equal (unsigned >=) *)
  | AE
[@@deriving equal, compare, hash, sexp, variants]
