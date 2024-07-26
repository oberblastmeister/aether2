open Core

type float_lit =
  { is_hex : bool
  ; integer : string option
  ; fraction : string option
  ; exponent : string option
  ; suffix : string option
  }
[@@deriving sexp_of]

module Encoding = struct
  type t =
    | None
    | Wide
    | Utf16
    | Utf32
    | Utf8
  [@@deriving sexp_of]

  let of_string = function
    | "" -> Some None
    | "L" -> Some Wide
    | "u" -> Some Utf16
    | "U" -> Some Utf32
    | "u8" -> Some Utf8
    | _ -> None
  ;;
end

type chr =
  | Chr of int
  | Esc of int64

type encoded_string =
  { s : string
  ; encoding : Encoding.t
  }
[@@deriving sexp_of]

type char_lit = encoded_string [@@deriving sexp_of]

type storage_spec =
  | Extern
  | Static
  | ThreadLocal
  | Auto
  | Register
  | Typedef
[@@deriving sexp_of]

type attr = | [@@deriving sexp_of]

type qual_spec =
  | Const
  | Restrict
  | Volatile
  | Atomic
  | Attr of attr
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
  | StructOrUnion of
      { kind : struct_or_union
      ; name : string option
      ; fields : field_group list
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
  | Array of
      { ty : decl_type
      ; specs : qual_spec list
      ; size : expr option
      }
  | Ptr of
      { specs : qual_spec list
      ; ty : decl_type
      }
  | Proto of
      { ty : decl_type
      ; params : param list
      ; variadic : bool
      }
  | ProtoOld of
      { ty : decl_type
      ; params : string list
      }
[@@deriving sexp_of]

and param =
  { specs : spec list
  ; name : string option
  ; ty : decl_type
  }
[@@deriving sexp_of]

and struct_decl =
  | BitField of
      { name : decl_name option
      ; size : expr
      }
  | Normal of decl_name
[@@deriving sexp_of]

and field_group =
  | Field of
      { specs : spec list
      ; decls : struct_decl list
      }
[@@deriving sexp_of]

and decl_name =
  { name : string
  ; ty : decl_type
  }
[@@deriving sexp_of]

(* The base type and the storage are common to all names. Each name might
 * contain type or storage modifiers *)
(* e.g.: int x, y; *)
(* like name_group, except the declared variables are allowed to have initializers *)
(* e.g.: int x=1, y=2; *)
and init_name_group =
  { specs : spec list
  ; init_names : init_name list
  }
[@@deriving sexp_of]

and init_name =
  { decl_name : decl_name
  ; init_expr : init_expr
  }
[@@deriving sexp_of]

and init_expr =
  | NoInit
  | SingleInit of expr
  | CompoundInit of init_item list
[@@deriving sexp_of]

and expr =
  | Unary of unary_op * expr
  | Binary of expr * bin_op * expr
  | Ternary of
      { cond : expr
      ; expr_then : expr
      ; expr_else : expr
      }
  | Cast of
      { ty : full_type
      ; init : init_expr
      }
  | Call of
      { func : expr
      ; args : expr list
      }
  | BuiltinVaArg of
      { expr : expr
      ; ty : full_type
      }
  | Int of string
  | Float of float_lit
  | Char of char_lit
  | String of encoded_string list
  | Variable of string
  | Index of
      { expr : expr
      ; index : expr
      }
  | Member of
      { expr : expr
      ; field : string
      }
  | DerefMember of
      { expr : expr
      ; field : string
      }
  | ExprSizeof of expr
  | TypeSizeof of full_type
[@@deriving sexp_of]

and unary_op =
  | Minus
  | Plus
  | Not
  | Bnot
  | Deref
  | Ref
  | Preincr
  | Predecr
  | Postincr
  | Postdecr
[@@deriving sexp_of]

and bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Band
  | Bor
  | Xor
  | Shl
  | Shr
  | E
  | Ne
  | Lt
  | Gt
  | Le
  | Ge
  | Assign
  | AddAssign
  | SubAssign
  | MulAssign
  | DivAssign
  | ModAssign
  | BandAssign
  | BorAssign
  | XorAssign
  | ShlAssign
  | ShrAssign
  | Comma
[@@deriving sexp_of]

and stmt =
  | Noop of Span.t
  | Def of decl
  | Expr of
      { expr : expr
      ; span : Span.t
      }
  | Block of
      { stmts : stmt list
      ; span : Span.t
      }
  | If of
      { cond : expr
      ; then_stmt : stmt
      ; else_stmt : stmt option
      ; span : Span.t
      }
  | While of
      { cond : expr
      ; body : stmt
      ; span : Span.t
      }
  | Switch of
      { cond : expr
      ; body : stmt
      ; span : Span.t
      }
  | DoWhile of
      { cond : expr
      ; body : stmt
      ; span : Span.t
      }
  | For of
      { clause : for_clause option
      ; cond : expr option
      ; update : expr option
      ; body : stmt
      ; span : Span.t
      }
[@@deriving sexp_of]

and for_clause =
  | ForExpr of expr
  | ForDecl of decl
[@@deriving sexp_of]

and decl =
  | DeclDef of init_name_group
  | FunDecl of
      { specs : spec list
      ; decl_name : decl_name
      ; kr_params : decl list
      ; body : stmt
      ; span : Span.t
      }
[@@deriving sexp_of]

and init_item =
  { (* list of places because places can be nested *)
    (* for example: *)
    (* struct B b = {.a.x = 0}; // valid C, invalid C++ (nested) *)
    place : init_place list
  ; expr : init_expr
  }
[@@deriving sexp_of]

(* also known as the designator *)
and init_place =
  | AtIndex of expr
  | AtField of string
[@@deriving sexp_of]

(* corresponds to type_name *)
and full_type =
  { specs : spec list
  ; ty : decl_type
  }
[@@deriving sexp_of]

let map_decl_name_ty (decl_name : decl_name) ~f = { decl_name with ty = f decl_name.ty }
