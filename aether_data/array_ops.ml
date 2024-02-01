open Core

module Option_array = struct
  module type Ops = sig
    val unsafe_get_some_assuming_some : 'a Option_array.t -> int -> 'a
    val unsafe_set_some : 'a Option_array.t -> int -> 'a -> unit
    val unsafe_swap : 'a Option_array.t -> int -> int -> unit
    val unsafe_set_none : 'a Option_array.t -> int -> unit
    val unsafe_is_some : 'a Option_array.t -> int -> bool
  end

  module Safe_ops : Ops = struct
    let[@inline always] unsafe_get_some_assuming_some = Option_array.get_some_exn
    let[@inline always] unsafe_set_some = Option_array.set_some
    let[@inline always] unsafe_swap = Option_array.swap
    let[@inline always] unsafe_set_none = Option_array.set_none
    let[@inline always] unsafe_is_some = Option_array.is_some
  end

  module Unsafe_ops : Ops = struct
    let[@inline always] unsafe_get_some_assuming_some =
      Option_array.unsafe_get_some_assuming_some
    ;;

    let[@inline always] unsafe_set_some = Option_array.unsafe_set_some

    let[@inline always] unsafe_swap t i j =
      let tmp = unsafe_get_some_assuming_some t i in
      unsafe_set_some t i (unsafe_get_some_assuming_some t j);
      unsafe_set_some t j tmp
    ;;

    let[@inline always] unsafe_set_none = Option_array.unsafe_set_none
    let[@inline always] unsafe_is_some = Option_array.unsafe_is_some
  end

  module Ops : Ops = Safe_ops
end
