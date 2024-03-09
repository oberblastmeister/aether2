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

module Name = struct
  module T = Entity.Name.Make ()
  module Map = Map.Make_using_comparator (T)
  module Set = Set.Make_using_comparator (T)
  module Table = Entity.Map.Make (T)
  include T
end

module Label = struct
  module T = Entity.Name.Make ()
  module Map = Map.Make_using_comparator (T)
  module Set = Set.Make_using_comparator (T)
  module Table = Entity.Map.Make (T)
  include T
end

module Stack_slot = struct
  module T = Entity.Name.Make ()
  module Map = Map.Make_using_comparator (T)
  module Set = Set.Make_using_comparator (T)
  module Table = Entity.Map.Make (T)
  include T
end
