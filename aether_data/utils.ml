open! Core

module Option_array = struct
  module OA = Option_array

  let resize_array a size =
    let a' = OA.create ~len:size in
    OA.blit ~src:a ~dst:a' ~src_pos:0 ~dst_pos:0 ~len:(OA.length a);
    a'
  ;;

  let resize_for_index a index =
    if index >= Option_array.length a
    then (
      let new_size = max 4 (Int.round_up (index + 1) ~to_multiple_of:2) in
      resize_array a new_size)
    else a
  ;;
end

module Make_quickcheck_generator_list_conv_gen (M : sig
    type 'a t
    type 'a elt [@@deriving quickcheck]

    val of_list : 'a elt list -> 'a t
    val to_list : 'a t -> 'a elt list
  end) : Quickcheckable.S1 with type 'a t := 'a M.t = struct
  type 'a t = 'a M.t

  let quickcheck_generator gen =
    Quickcheck.Generator.map ~f:M.of_list
    @@ List.quickcheck_generator
    @@ M.quickcheck_generator_elt gen
  ;;

  let quickcheck_observer ob =
    Quickcheck.Observer.unmap ~f:M.to_list
    @@ List.quickcheck_observer
    @@ M.quickcheck_observer_elt ob
  ;;

  let quickcheck_shrinker sh =
    Quickcheck.Shrinker.map ~f:M.of_list ~f_inverse:M.to_list
    @@ List.quickcheck_shrinker
    @@ M.quickcheck_shrinker_elt sh
  ;;
end

module Make_quickcheck_list_conv (M : sig
    type 'a t

    val of_list : 'a list -> 'a t
    val to_list : 'a t -> 'a list
  end) =
Make_quickcheck_generator_list_conv_gen (struct
    include M

    type 'a t = 'a M.t
    type 'a elt = 'a [@@deriving quickcheck]
  end)

module Make_quickcheck_list_conv0 (M : sig
    type t
    type elt [@@deriving quickcheck]

    val of_list : elt list -> t
    val to_list : t -> elt list
  end) : Quickcheckable.S with type t := M.t = struct
  module Q = Make_quickcheck_generator_list_conv_gen (struct
      type 'a t = M.t
      type 'a elt = M.elt [@@deriving quickcheck]

      let of_list = M.of_list
      let to_list = M.to_list
    end)

  let quickcheck_generator = Q.quickcheck_generator M.quickcheck_generator_elt
  let quickcheck_observer = Q.quickcheck_observer M.quickcheck_observer_elt
  let quickcheck_shrinker = Q.quickcheck_shrinker M.quickcheck_shrinker_elt
end
