open! O
open Entity

module Control : sig
  type e = E
  type o = O
  type c = C
end

module SControl : sig
  type 'a t =
    | SO : Control.o t
    | SC : Control.c t
    | SE : Control.e t
end

module Name : Name_id.S
module Label : Name_id.S
