open Core

let get_lexing_position lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line = p.Lexing.pos_lnum in
  let col = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  Position.{ line; col }
;;

type error =
  { pos : Position.t option
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
  let lexer = Staged.unstage (Lexer.lexer ~is_typedefname:Context.is_typedefname) in
  match
    Parser.parse
      lexbuf
      lexer
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
  print_s [%sexp (ast : (Ast.decl list, error) Result.t)];
  [%expect.unreachable];
  ()
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (TODO aether_frontend/parser_generated.mly:650:44)
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
  Called from Base__Error.raise_s in file "src/error.ml", line 10, characters 19-40
  Called from Aether_frontend__Parser_generated.Make.Tables.semantic_action.(fun) in file "aether_frontend/parser_generated.mly", line 650, characters 39-51
  Called from MenhirLib.Engine.Make.reduce in file "lib/pack/menhirLib.ml", line 1431, characters 16-42
  Called from Aether_frontend__Parser.Make.parse in file "aether_frontend/parser.ml", line 31, characters 23-42
  Called from Aether_frontend__Parser.parse in file "aether_frontend/parser.ml", line 50, characters 4-118
  Called from Aether_frontend__Parser.(fun) in file "aether_frontend/parser.ml", line 66, characters 12-19
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

let check_lexer s =
  let lexbuf = Lexing.from_string s in
  let lexer = Staged.unstage (Lexer.lexer ~is_typedefname:(fun _ -> false)) in
  let rec go acc =
    match lexer lexbuf with
    | Token.EOF -> List.rev acc
    | t -> go (t :: acc)
  in
  let toks = go [] in
  print_s [%sexp (toks : Token.t list)]
;;

let%expect_test _ =
  let s = {|
  poiuafd 
  |} in
  check_lexer s;
  [%expect {| ((NAME poiuafd) VARIABLE) |}];
  ()
;;

let%expect_test _ =
  check_lexer {|
    asdf 1324 adsf "adsfa" void 'a' 'x' "\\x\n\tx\"\'"
  |};
  [%expect
    {|
    ((NAME asdf) VARIABLE (INT_LITERAL 1324) (NAME adsf) VARIABLE
     (STRING_LITERAL ((s adsfa) (encoding None))) VOID
     (CHAR_LITERAL ((s a) (encoding None)))
     (CHAR_LITERAL ((s x) (encoding None)))
     (STRING_LITERAL ((s  "\\x\
                         \n\tx\"'") (encoding None)))) |}]
;;
