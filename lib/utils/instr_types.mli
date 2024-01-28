open! O

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

module Name : Entity.Name.S
module Label : Entity.Name.S
