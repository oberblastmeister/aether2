open Core

type storage_spec =
  | Extern
  | Static
  | ThreadLocal
  | Auto
  | Register
[@@deriving sexp_of]

type qual_spec =
  | Const
  | Restrict
  | Volatile
  | Atomic
[@@deriving sexp_of]

type func_spec =
  | Inline
  | Noreturn
[@@deriving sexp_of]

and type_spec =
  | Void
  | Char
  | Short
  | Int
  | Long
  | Float
  | Double
  | Signed
  | Unsigned
  | Bool
  | Named of string
  | StructUnion of
      { kind : struct_or_union
      ; name : string option
      ; fields : field list option
      }
[@@deriving sexp_of]

and spec =
  | TypeSpec of type_spec
  | FuncSpec of func_spec
  | QualSpec of qual_spec
  | StorageSpec of storage_spec
[@@deriving sexp_of]

and struct_or_union =
  | Struct
  | Union
[@@deriving sexp_of]

and decl_type =
  | JustBase
  | Array of { decl_type : decl_type }
[@@deriving sexp_of]

and field = Field of { specs : spec list } [@@deriving sexp_of]
