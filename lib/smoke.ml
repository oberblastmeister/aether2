module X : sig
  type t

  val mk : t
  val id : t -> t
end = struct
  type t = int

  let mk = 0
  let id x = x
end

module Y : module type of X = struct
  type t = bool

  let mk = true
  let id x = x
end

module type Stack = sig
  type 'a t

  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a option
end

let f x =
  match x with
  | 0 -> 0
  | n ->
    (match n with
     | 0 -> 0
     | n -> 1)
;;

(* type 'a expr =
  | Var of 'a
  | Lam of 'a * 'a expr
  | App of 'a expr * 'a expr
  | Int of int *)
