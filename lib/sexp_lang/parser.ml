open! O

type 'a t = Cst.t -> 'a

module Pos = struct
  type t = int [@@deriving equal, compare, sexp]
end

module R = Algaeff.Reader.Make (Span)

module Error = struct
  type t =
    | Single of
        { span : Span.t
        ; message : Sexp.t
        }
    | List of t list
  [@@deriving equal, compare, sexp]

  let to_error t = Error.create_s [%sexp "parse error", (t : t)]
end

exception Error of Error.t

let run f =
  try Result.Ok (R.run ~env:Span.empty f) with
  | Error e -> Result.Error e
;;

let with_span (span : Span.t) f = R.scope (const span) f
let parse_error message = raise (Error (Single { span = R.read (); message }))

let atom f = function
  | Cst.Atom s -> with_span s.span (fun () -> f s.value)
  | Cst.List l ->
    R.scope (const l.span) (fun () ->
      parse_error [%message "expected atom" ~got:(l.items : Cst.t list)])
;;

let string = atom Fn.id

let list sexp f =
  match sexp with
  | Cst.List x -> R.scope (const x.span) (fun () -> f x.items)
  | Cst.Atom x -> parse_error [%message "expected list" ~got:(x.value : string)]
;;

let list_ref sexp f = list sexp (fun xs -> f (ref xs))

let item list_ref f =
  match !list_ref with
  | [] -> parse_error [%message "unexpected empty list"]
  | x :: xs ->
    let res = R.scope (const (Cst.span x)) (fun () -> f x) in
    list_ref := xs;
    res
;;

let optional_item list f =
  match list with
  | [] -> None
  | x :: [] -> with_span (Cst.span x) (fun () -> Some (f x))
  | _ -> None
;;

let rest xs f = List.map xs ~f

module Syntax = struct
  let ( <$> ) f p sexp = f (p sexp)

  let ( <|> ) p1 p2 sexp =
    match p1 sexp with
    | exception Error e1 ->
      (match p2 sexp with
       | exception Error e2 -> raise (Error (List [ e1; e2 ]))
       | x -> x)
    | x -> x
  ;;
end

let either p1 p2 sexp = Syntax.(Either.first <$> p1 <|> (Either.second <$> p2)) sexp
