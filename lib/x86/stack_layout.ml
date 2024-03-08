open O
open Types
open Utils.Instr_types

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
let size t = t.stack_size

let create stack_instrs =
  let locals_size = ref 0l in
  let offset_of_local = Hashtbl.create (module Name) in
  let end_size = ref 0l in
  List.iter stack_instrs ~f:(function
    | Stack_instr.ReserveEnd { size } -> end_size := max !end_size size
    | ReserveLocal { name; size } ->
      Hashtbl.add_exn offset_of_local ~key:name ~data:!locals_size;
      locals_size := !locals_size + size);
  (* Vec.iter locals ~f:(fun (name, size) ->
     Hashtbl.add_exn offset_of_local ~key:name ~data:!locals_size;
     locals_size := !locals_size + size); *)
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
