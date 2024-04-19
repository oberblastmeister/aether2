open O

open struct
  module Entity = Aether_entity
  module Id = Entity.Id
  module Name = Entity.Name
end

module type Table = sig
  type 'a t
  type key

  val create : unit -> 'a t
  val find : 'a t -> key -> 'a option
  val add_exn : 'a t -> key:key -> data:'a -> unit
end

module type S = sig
  type 'k t
  type key

  val create : ?next_id:'k Id.t -> unit -> 'k t
  val id_of_key : 'k t -> key -> 'k Id.t
  val get_next_id : 'k t -> 'k Id.t
end

module Make (Table : Table) : S with type key = Table.key = struct
  type 'k t =
    { mutable id : 'k Id.t
    ; id_of_key : 'k Id.t Table.t
    }

  type key = Table.key

  let create ?(next_id = Id.initial) () = { id = next_id; id_of_key = Table.create () }

  let id_of_key t s =
    match Table.find t.id_of_key s with
    | Some id -> id
    | None ->
      let id = t.id in
      t.id <- Id.next id;
      Table.add_exn t.id_of_key ~key:s ~data:id;
      id
  ;;

  let get_next_id t = t.id
end

module Make_from_id_table (Id_table : Entity.Map.S) = Make (struct
    type 'v t = 'v Id_table.t
    type key = Id_table.k

    let create () = Id_table.create ()
    let find = Id_table.find
    let add_exn = Id_table.add_exn
  end)

module Make_from_hash (T : Hashtbl.Key) = struct
  open struct
    module Table = Hashtbl.Make_plain (T)
  end

  include Make (struct
      type 'v t = 'v Table.t
      type key = T.t

      let create () = Table.create ()
      let find = Hashtbl.find
      let add_exn = Hashtbl.add_exn
    end)
end

module String_intern = struct
  include Make (struct
      type 'v t = 'v String.Table.t
      type key = string

      let create () = String.Table.create ()
      let find = Hashtbl.find
      let add_exn = Hashtbl.add_exn
    end)

  let name_of_key t s = Entity.Name.create s (id_of_key t s)
end
