open O

module Option_array = struct
  module type Ops = sig
    val unsafe_get_some_assuming_some : 'a Option_array.t -> int -> 'a
    val unsafe_set_some : 'a Option_array.t -> int -> 'a -> unit
  end

  module Safe_ops : Ops = struct
    let[@inline always] unsafe_get_some_assuming_some = Option_array.get_some_exn
    let[@inline always] unsafe_set_some = Option_array.set_some
  end

  module Unsafe_ops : Ops = struct
    let[@inline always] unsafe_get_some_assuming_some =
      Option_array.unsafe_get_some_assuming_some
    ;;

    let[@inline always] unsafe_set_some = Option_array.unsafe_set_some
  end
end
