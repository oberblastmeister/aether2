type storage_class =
  | Extern
  | Static
  | ThreadLocal
  | Auto
  | Register
[@@deriving sexp]
