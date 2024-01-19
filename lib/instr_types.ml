open! O

module Control = struct
  type e = E
  type o = O
  type c = C
end

module SControl = struct
  type 'a t =
    | SO : Control.o t
    | SC : Control.c t
    | SE : Control.e t
end

module Name = Name_id.Make ()
module Label = Name_id.Make ()
