open Core
open Lexer_utils

let string_of_uchar (u : int) =
  let x = u in
  String.of_list
  @@
  if x <= 0x007F
  then [ Char.of_int_exn x ]
  else if x <= 0x07FF
  then [ Char.of_int_exn (0x80 lor (x land 0x3F)); Char.of_int_exn (0xC0 lor (x lsr 6)) ]
  else if x <= 0xFFFF
  then
    [ Char.of_int_exn (0x80 lor (x land 0x3F))
    ; Char.of_int_exn (0x80 lor ((x lsr 6) land 0x3F))
    ; Char.of_int_exn (0xE0 lor (x lsr 12))
    ]
  else
    [ Char.of_int_exn (0x80 lor (x land 0x3F))
    ; Char.of_int_exn (0x80 lor ((x lsr 6) land 0x3F))
    ; Char.of_int_exn (0x80 lor ((x lsr 12) land 0x3F))
    ; Char.of_int_exn (0xF0 lor (x lsr 18))
    ]
;;
(* Handling of characters and escapes in string and char constants

   let check_utf8 min x =
   if x > 0x10FFFF || (x >= 0xD800 && x <= 0xDFFF)
   then lex_error (sprintf "Wrong Unicode value U+%X" x);
   if x < min then lex_error (sprintf "Overlong UTF-8 encoding for Unicode value U+%X" x);
   Chr x
   ;;

   let check_universal_character x =
   if x > 0x10FFFF
   || (x >= 0xD800 && x <= 0xDFFF)
   || (x < 0xA0 && x <> 0x24 && x <> 0x40 && x <> 0x60)
   then lex_error (sprintf "Wrong universal character name U+%X" x)
   else Chr x
   ;;

   let add_char_utf8 x accu =
   if x <= 0x007F
   then Int64.of_int x :: accu
   else if x <= 0x07FF
   then Int64.of_int (0x80 lor (x land 0x3F)) :: Int64.of_int (0xC0 lor (x lsr 6)) :: accu
   else if x <= 0xFFFF
   then
   Int64.of_int (0x80 lor (x land 0x3F))
   :: Int64.of_int (0x80 lor ((x lsr 6) land 0x3F))
   :: Int64.of_int (0xE0 lor (x lsr 12))
   :: accu
   else
   Int64.of_int (0x80 lor (x land 0x3F))
   :: Int64.of_int (0x80 lor ((x lsr 6) land 0x3F))
   :: Int64.of_int (0x80 lor ((x lsr 12) land 0x3F))
   :: Int64.of_int (0xF0 lor (x lsr 18))
   :: accu
   ;;

   let add_char_utf16 x accu =
   if x <= 0xFFFF
   then Int64.of_int x :: accu
   else (
   let x = x - 0x10000 in
   Int64.of_int (0xDC00 lor (x land 0x3FF))
   :: Int64.of_int (0xD800 lor (x lsr 10))
   :: accu)
   ;;

   let add_char (enc : Token.Encoding.t) c accu =
   match c, enc with
   | Esc x, _ ->
   (* Escapes are never encoded *)
   x :: accu
   | Chr x, (None | Utf8) ->
   (* Characters are encoded in UTF8 *)
   add_char_utf8 x accu
   | Chr x, Utf16 ->
   (* Characters are encoded in UTF16 *)
   add_char_utf16 x accu
   | Chr x, Utf32 ->
   (* Characters are not encoded *)
   Int64.of_int x :: accu
   | Chr x, Wide ->
   (* Depends on size of wchar_t *)
   if Cutil.sizeof_ikind (Cutil.wchar_ikind ()) = 2
   then add_char_utf16 x accu
   else Int64.of_int x :: accu
   ;; *)
