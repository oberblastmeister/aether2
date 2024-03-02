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
[@@deriving equal, compare, hash, sexp, variants]
