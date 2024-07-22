open Core

type storage_spec =
  | Extern
  | Static
  | ThreadLocal
  | Auto
  | Register
  | Typedef
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
  | Array of
      { ty : decl_type
      ; qual_spec : qual_spec list
      ; size : expr option
      }
  | Ptr of
      { qual_spec : qual_spec list
      ; ty : decl_type
      }
  | Proto of
      { ty : decl_type
      ; params : parameter list
      ; variadic : bool
      }
[@@deriving sexp_of]

and parameter =
  { specs : spec list
  ; name : string option
  ; ty : decl_type
  }
[@@deriving sexp_of]

and field = Field of { specs : spec list } [@@deriving sexp_of]

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
[@@deriving sexp_of]

and expr =
  | Unary of unary_op * expr
  | Bin of expr * bin_op * expr
  | Ternary of
      { cond : expr
      ; expr_then : expr
      ; expr_else : expr
      }
[@@deriving sexp_of]

and unary_op =
  | Minus
  | Plus
  | Not
  | Bnot
  | Memof
  | Addrof
  | Preincr
  | Predecr
  | Posincr
  | Posdecr
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
  | Eq
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
      ; params : decl list
      ; body : stmt
      ; span : Span.t
      }
[@@deriving sexp_of]

let map_decl_name_ty (decl_name : decl_name) ~f = { decl_name with ty = f decl_name.ty }
