open O
include Name_id_intf

module Make () = struct
  module Id = Id.Make ()

  module T = struct
    type t = Id.key Name_id_intf.t [@@deriving equal, compare, hash, sexp]
  end

  include T

  let create name id = { name; id }
  let of_string_global_unique name = { name; id = Id.of_global_unique () }

  include Comparable.Make (T)
end

let to_dotted_string name_id =
  name_id.name ^ "." ^ string_of_int (Raw_id.to_int name_id.id)
;;
