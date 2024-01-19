open! O
include Id_intf

module Make () = struct
  include Raw_id

  type key [@@deriving equal, compare, hash, sexp]

  module T = struct
    type t = key Raw_id.t [@@deriving equal, compare, hash, sexp]
  end

  include T

  module Gen = struct
    include Raw_id.Gen

    type id = T.t
    type t = key Raw_id.Gen.t
  end

  let of_raw raw = raw
  let to_raw t = t

  include Comparable.Make (T)
end
