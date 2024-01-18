open! O

type 'a t =
  { compare : 'a -> 'a -> int
  ; mutable data : 'a array
  ; mutable size : int
  }
