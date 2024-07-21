open Core

type pos =
  { line : int
  ; col : int
  }
[@@deriving sexp_of]

let get_lexing_position lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line = p.Lexing.pos_lnum in
  let col = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  { line; col }
;;

type error =
  { pos : pos option
  ; msg : string
  }
[@@deriving sexp_of]

exception Exn of error

module Make (Context : Context.S) = struct
  module Generated = Parser_generated.Make (Context)
  module I = Generated.MenhirInterpreter

  let rec parse lexbuf lexer (checkpoint : _ I.checkpoint) =
    match checkpoint with
    | I.InputNeeded _env ->
      let token = lexer lexbuf in
      let startp = lexbuf.Lexing.lex_start_p
      and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      parse lexbuf lexer checkpoint
    | I.Shifting _ | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse lexbuf lexer checkpoint
    | I.HandlingError _env ->
      let pos = get_lexing_position lexbuf in
      raise (Exn { pos = Some pos; msg = "syntax error" })
    | I.Accepted v -> v
    | I.Rejected ->
      raise (Exn { pos = None; msg = "invalid syntax (parser rejected the input)" })
  ;;
end

let parse s =
  let module Context = Context.Make () in
  let module Parser = Make (Context) in
  let lexbuf = Lexing.from_string s in
  match
    Parser.parse
      lexbuf
      (Lexer.lexer ~is_typedefname:Context.is_typedefname)
      (Parser.Generated.Incremental.translation_unit_file lexbuf.lex_curr_p)
  with
  | exception Exn e -> Error e
  | exception Lexer_utils.Exn e ->
    let pos = get_lexing_position lexbuf in
    Error { pos = Some pos; msg = e }
  | ast -> Ok ast
;;

let%expect_test _ =
  let s = {|
  int main(char, char, int);
  |} in
  let ast = parse s in
  print_s [%sexp (ast : (unit, error) Result.t)];
  [%expect];
  ()
;;
