open Core

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

let check_parser s =
  let ast = Parser.parse s in
  print_s [%sexp (ast : (Ast.decl list, Parser.error) Result.t)]
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

let%expect_test _ =
  check_parser {|
void main() {
  "afdsdf";
}
  |};
  [%expect{|
    (Ok
     ((FunDecl (specs ((TypeSpec Void)))
       (decl_name ((name main) (ty (ProtoOld (ty JustBase) (params ())))))
       (kr_params ())
       (body
        (Block
         (stmts
          ((Expr (expr (String (((s afdsdf) (encoding None)))))
            (span ((start ((line 0) (col 0))) (stop ((line 0) (col 0))))))))
         (span ((start ((line 0) (col 0))) (stop ((line 0) (col 0)))))))
       (span ((start ((line 0) (col 0))) (stop ((line 0) (col 0)))))))) |}]
;;

let%expect_test _ =
  check_parser {|
int main(void) {
  10;
}
  |};
  [%expect
    {|
    (Ok
     ((FunDecl (specs ((TypeSpec Int)))
       (decl_name
        ((name main)
         (ty
          (Proto (ty JustBase)
           (params (((specs ((TypeSpec Void))) (name ()) (ty JustBase))))
           (variadic false)))))
       (kr_params ())
       (body
        (Block
         (stmts
          ((Expr (expr (Int 10))
            (span ((start ((line 0) (col 0))) (stop ((line 0) (col 0))))))))
         (span ((start ((line 0) (col 0))) (stop ((line 0) (col 0)))))))
       (span ((start ((line 0) (col 0))) (stop ((line 0) (col 0)))))))) |}]
;;
