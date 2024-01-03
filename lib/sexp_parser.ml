open O

module Pos = struct
  type t = int [@@deriving equal, compare, sexp]
end

module R = Algaeff.Reader.Make (Span)

module Error = struct
  type t = { span : Span.t; message : Sexp.t } [@@deriving equal, compare, sexp]

  let to_error { span; message } =
    Error.create_s [%sexp "parse error", (span : Span.t), (message : Sexp.t)]
end

exception Error of Error.t

let run f =
  try Result.Ok (R.run ~env:Span.empty f) with Error e -> Result.Error e

let with_span (span : Span.t) f = R.scope (const span) f
let parse_error message = raise (Error { span = R.read (); message })

let atom f = function
  | Sexp_cst.Atom s -> with_span s.span (fun () -> f s.value)
  | Sexp_cst.List l ->
      R.scope (const l.span) (fun () ->
          parse_error
            [%message "expected atom" ~got:(l.items : Sexp_cst.t list)])

let list f = function
  | Sexp_cst.List x -> R.scope (const x.span) (fun () -> f x.items)
  | Sexp_cst.Atom x ->
      parse_error [%message "expected list" ~got:(x.value : string)]

let list_ref f = list (fun xs -> f (ref xs))

let item list_ref f =
  match !list_ref with
  | [] -> parse_error [%message "unexpected empty list"]
  | x :: xs ->
      let res = R.scope (const (Sexp_cst.span x)) (fun () -> f x) in
      list_ref := xs;
      res

let optional_item list f =
  match list with
  | [] -> None
  | x :: [] -> with_span (Sexp_cst.span x) (fun () -> Some (f x))
  | _ -> None

let rest xs f = List.map xs ~f
