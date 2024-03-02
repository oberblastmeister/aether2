type t =
  | RAX
  | RCX
  | RDX
  | RBX
  | RSP
  | RBP
  | RSI
  | RDI
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
[@@deriving enum, equal, compare, sexp, hash, variants]
