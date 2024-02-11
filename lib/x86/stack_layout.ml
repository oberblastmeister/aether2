open O
open Types
open Utils.Instr_types

module Stack_layout : sig
  type t [@@deriving sexp_of]

  val create : VReg.t Function.t -> t
  val end_offset : t -> int32 -> int32
  val local_offset : t -> Name.t -> int32
end = struct
  type t =
    { stack_size : int32
    ; offset_of_local : (Name.t, int32) Hashtbl.t
    ; locals_offset : int32
    ; end_offset : int32
    }
  [@@deriving sexp_of]

  open Int32

  let end_offset t i = t.end_offset + i
  let local_offset t name = t.locals_offset + Hashtbl.find_exn t.offset_of_local name

  let create fn =
    let locals_size = ref 0l in
    let offset_of_local = Hashtbl.create (module Name) in
    let end_size = ref 0l in
    F.Fold.(Function.instrs_forward_fold @> of_fn Instr.get_virt @> FC.Option.fold)
      fn
      (fun instr ->
         (match instr with
          | VInstr.ReserveStackEnd { size } -> end_size := max !end_size size
          | VInstr.ReserveStackLocal { name; size } ->
            Hashtbl.add_exn offset_of_local ~key:name ~data:!locals_size;
            locals_size := !locals_size + size;
            ()
          | _ -> ());
         ());
    let align size =
      assert (size % 8l = 0l);
      if size % 16l = 8l then size + 8l else size
    in
    { stack_size = align @@ (!locals_size + !end_size)
    ; end_offset = 0l
    ; locals_offset = !end_size
    ; offset_of_local
    }
  ;;
end
