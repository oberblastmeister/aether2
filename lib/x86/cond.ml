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
  (* greater (signed >) *)
  | G
  (* greater or equal (signed >=) *)
  | GE
  (* less (signed <) *)
  | L
  (* less or equal (signed <=) *)
  | LE
[@@deriving equal, compare, hash, sexp, variants]
