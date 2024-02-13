open! Core
include Name_intf

module F = struct
  let create name id = { name; id }
  let of_string_global_unique name = { name; id = Id.of_global_unique () }
  let to_id { id; _ } = id
  let to_int { id; _ } = Id.to_int id
end

include F

module Make () = struct
  module Id = Id.Make ()

  type key = Id.key [@@deriving equal, compare, hash, sexp]

  module T = struct
    type t = Id.key Name_intf.t [@@deriving equal, compare, hash, sexp]
  end

  include T
  include F
  module C = Base.Comparable.Make (T)
  include C
end
