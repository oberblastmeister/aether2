open Core

type t =
  | NAME of string
  | VARIABLE
  | TYPE
  | CONSTANT
  | INT_LITERAL of string
  | CHAR_LITERAL of Ast.encoded_string
  | STRING_LITERAL of Ast.encoded_string
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
