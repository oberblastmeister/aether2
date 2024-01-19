open O
include Name_id_intf

module Make () = struct
  module Id = Id.Make ()

  type key = Id.key [@@deriving equal, compare, hash, sexp]

  module T = struct
    type t = Id.key Name_id_intf.t [@@deriving equal, compare, hash, sexp]
  end

  include T

  let create name id = { name; id }
  let of_string_global_unique name = { name; id = Id.of_global_unique () }
  let to_id { id; _ } = id
  let to_raw { id; _ } = Id.to_raw id

  include Comparable.Make (T)
end

let to_dotted_string (name_id : _ t) =
  name_id.name ^ "." ^ string_of_int (Raw_id.to_int @@ Id.to_raw name_id.id)
;;
