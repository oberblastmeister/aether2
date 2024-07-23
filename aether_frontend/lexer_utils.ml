exception Exn of string 

let lex_error s = raise (Exn s)
