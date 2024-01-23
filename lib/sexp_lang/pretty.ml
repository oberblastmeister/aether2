open! O
include Pretty_intf

let indent_increase = 2

let add_newline buffer indent =
  Buffer.add_char buffer '\n';
  Buffer.add_string buffer (String.make indent ' ')
;;

let print_ann buffer indent = function
  | Line ->
    add_newline buffer indent;
    indent
  | IndentLine ->
    let indent = indent + indent_increase in
    add_newline buffer indent;
    indent
;;

let print_to_buffer sexp =
  let buffer = Buffer.create 10 in
  let rec go indent = function
    | Atom s -> Buffer.add_string buffer s
    | List items ->
      Buffer.add_char buffer '(';
      go_list indent items;
      Buffer.add_char buffer ')'
    | Ann _ -> ()
  and go_list indent = function
    | Ann ann :: xs ->
      let indent = print_ann buffer indent ann in
      go_list indent xs
    | x :: Ann ann :: xs ->
      go indent x;
      let indent = print_ann buffer indent ann in
      go_list indent xs
    | x :: [] -> go indent x
    | x :: xs ->
      go indent x;
      Buffer.add_char buffer ' ';
      go_list indent xs
    | [] -> ()
  in
  go 0 sexp;
  buffer
;;

let to_string sexp = print_to_buffer sexp |> Buffer.contents

let%expect_test _ =
  let sexp =
    List
      [ Atom "define"
      ; List [ Atom "fun"; Atom "b"; Atom "c" ]
      ; Ann IndentLine
      ; List [ Atom "let"; Atom "x" ]
      ; Ann Line
      ; List [ Atom "let"; Atom "y" ]
      ; Ann Line
      ; List [ Atom "label"; Atom "block"; Ann IndentLine; List [ Atom "stuff" ] ]
      ; Ann Line
      ; List [ Atom "label"; Atom "block2"; Ann IndentLine; List [ Atom "more_stuff" ] ]
      ]
  in
  to_string sexp |> print_endline;
  [%expect
    {|
    (define (fun b c)
      (let x)
      (let y)
      (label block
        (stuff))
      (label block2
        (more_stuff))) |}]
;;
