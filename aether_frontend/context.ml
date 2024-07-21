open Core

module type S = sig
  (* This declares [id] as a typedef name. *)
  val declare_typedefname : string -> unit

  (* This declares [id] as a variable (hence un-declares it as a typedef name). *)
  val declare_varname : string -> unit

  (* This tests whether [id] is known as a typedef name. *)
  val is_typedefname : string -> bool

  (* A context is just a set of identifiers. It is the set of typedef
     names that are now visible. *)
  type context

  (* This takes a snapshot of the current context. *)
  val save_context : unit -> context

  (* This re-installs a snapshot as the current context. *)
  val restore_context : context -> unit
end

module Make () : S = struct
  (* This mutable global variable stores the current context. *)
  let current = ref String.Set.empty

  (* This declares [id] as a typedef name. *)
  let declare_typedefname id = current := Set.add !current id

  (* This declares [id] as a variable (hence un-declares it as a typedef name). *)
  let declare_varname id = current := Set.remove !current id

  (* This tests whether [id] is known as a typedef name. *)
  let is_typedefname id = Set.mem !current id

  (* A context is just a set of identifiers. It is the set of typedef
     names that are now visible. *)
  type context = String.Set.t

  (* This takes a snapshot of the current context. *)
  let save_context () = !current

  (* This re-installs a snapshot as the current context. *)
  let restore_context snapshot = current := snapshot
end
