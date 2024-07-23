open Core

type float =
  { is_hex : bool
  ; integer : string option
  ; fraction : string option
  ; exponent : string option
  ; suffix : string option
  }
[@@deriving sexp_of]

type const =
  | Int of string
  | Float of float
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

type t =
  | NAME of string
  | VARIABLE
  | TYPE
  | CONSTANT
  | CHAR_LITERAL of encoded_string
  | STRING_LITERAL of encoded_string
  | ALIGNAS
  | ALIGNOF
  | ATOMIC
  | AUTO
  | BOOL
  | BREAK
  | CASE
  | CHAR
  | COMPLEX
  | CONST
  | CONTINUE
  | DEFAULT
  | DO
  | DOUBLE
  | ELSE
  | ENUM
  | EXTERN
  | FLOAT
  | FOR
  | GENERIC
  | GOTO
  | IF
  | IMAGINARY
  | INLINE
  | INT
  | LONG
  | NORETURN
  | REGISTER
  | RESTRICT
  | RETURN
  | SHORT
  | SIGNED
  | SIZEOF
  | STATIC
  | STATIC_ASSERT
  | STRUCT
  | SWITCH
  | THREAD_LOCAL
  | TYPEDEF
  | UNION
  | UNSIGNED
  | VOID
  | VOLATILE
  | WHILE
  | ELLIPSIS
  | ADD_ASSIGN
  | SUB_ASSIGN
  | MUL_ASSIGN
  | DIV_ASSIGN
  | MOD_ASSIGN
  | OR_ASSIGN
  | AND_ASSIGN
  | XOR_ASSIGN
  | LEFT_ASSIGN
  | RIGHT_ASSIGN
  | LEFT
  | RIGHT
  | EQEQ
  | NEQ
  | LEQ
  | GEQ
  | EQ
  | LT
  | GT
  | INC
  | DEC
  | PTR
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | PERCENT
  | BANG
  | ANDAND
  | BARBAR
  | AND
  | BAR
  | HAT
  | QUESTION
  | COLON
  | TILDE
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | LPAREN
  | RPAREN
  | SEMICOLON
  | COMMA
  | DOT
  (* ATOMIC_LPAREN is "special"; it's used for left parentheses that
     follow the ["_Atomic"] keyword. It isn't given a token alias *)
  | ATOMIC_LPAREN
  | EOF
[@@deriving sexp_of]

type token = t

let unescape c =
  Char.of_int_exn
  @@
  match c with
  | 'a' -> 7 (* bell *)
  | 'b' -> 8 (* backspace *)
  | 'e' -> 27 (* escape (GCC extension) *)
  | 'f' -> 12 (* form feed *)
  | 'n' -> 10 (* new line *)
  | 'r' -> 13 (* carriage return *)
  | 't' -> 9 (* horizontal tab *)
  | 'v' -> 11 (* vertical tab *)
  | c -> Char.to_int c
;;
