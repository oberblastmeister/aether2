open! O
include Id_intf

module T = struct
  include Raw_id

  type 'k t = Raw_id.t [@@deriving equal, compare, sexp, hash]

  let of_raw raw = raw
  let to_raw t = t
end

include T

module type S = S' with type 'k t' := 'k t

module Make () = struct
  include Raw_id

  type key [@@deriving equal, compare, hash, sexp]

  module T = struct
    type t = key T.t [@@deriving equal, compare, hash, sexp]
  end

  include T

  let of_raw raw = raw
  let to_raw t = t

  include Comparator.Make (T)
end
