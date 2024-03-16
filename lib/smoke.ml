(* module X : sig
   type t

   val mk : t
   val id : t -> t
   end = struct
   type t = int

   let mk = 0
   let id x = x
   end

   module Y : module type of X = struct
   type t = bool

   let mk = true
   let id x = x
   end

   module type Stack = sig
   type 'a t

   val push : 'a t -> 'a -> unit
   val pop : 'a t -> 'a option
   end

   let f x =
   match x with
   | 0 -> 0
   | n ->
   (match n with
   | 0 -> 0
   | n -> 1)
   ;; *)

(* type 'a expr =
   | Var of 'a
   | Lam of 'a * 'a expr
   | App of 'a expr * 'a expr
   | Int of int *)

(* precondition, mov must be legalized*)
(* let lower_stack_off ~cx off =
  let open Stack_off in
  match off with
  | End i -> Stack_layout.end_offset cx.stack_layout i
  | Local name -> Stack_layout.local_offset cx.stack_layout name
;;

let lower_imm ~cx (imm : VReg.t Imm.t) : Mach_reg.t Imm.t =
  let open Imm in
  match imm with
  | Int i -> Int i
  | Stack off -> Int (lower_stack_off ~cx off)
;;

let lower_address ~cx ~can_use_scratch address =
  let open Address in
  let vreg_is_spilled reg =
    match Ra.Allocation.find_exn cx.allocation reg.VReg.name with
    | Spilled -> true
    | InReg _ -> false
  in
  let base_is_spilled (base : _ Base.t) =
    match base with
    | Reg reg -> vreg_is_spilled reg
    | None | Rip -> false
  in
  let index_is_spilled (index : _ Index.t) =
    match index with
    | None -> false
    | Some { index; _ } -> vreg_is_spilled index
  in
  let move_base_scratch (base : _ Base.t) =
    match base with
    | Reg reg -> ()
    | None | Rip -> ()
  in
  (* match address with
  | Imm { offset; scale } -> Imm { offset = lower_imm ~cx offset; scale }
  | Complex { base; index; offset }
    when base_is_spilled base && index_is_spilled index && can_use_scratch ->
    
       ()
  | Complex { base; index; offset } -> () *)
  ()
;; *)

(* if can_use_scratch then (

   ) else () *)

(* let apply_allocation_address ~cx size address =
  let r11 = Cx.r11 cx size in
  match address with
  | Address.Complex { base; index; offset } ->
    let stack_slot_address = Cx.fresh_name cx "spill_address" |> Address.stack_local in
    let stack_slot = Operand.Mem stack_slot_address in
    (match base with
     | None ->
       Cx.add_minstr cx @@ Mov { s = size; dst = Reg r11; src = Imm offset };
       ()
     | Reg r ->
       Cx.add_minstr cx @@ Mov { s = size; dst = Reg r11; src = Cx.apply_vreg cx r };
       Cx.add_minstr cx
       @@ Add { s = size; dst = stack_slot; src1 = Reg r11; src2 = Imm offset };
       ()
     | Rip ->
       (* need to do this with the offset *)
       Cx.add_minstr cx
       @@ Mov { s = size; dst = Reg r11; src = Operand.Mem (Address.rip_relative offset) };
       ());
    Cx.add_minstr cx @@ Mov { s = size; dst = stack_slot; src = Reg r11 };
    (match index with
     | None -> ()
     | Some { index; scale } ->
       Cx.add_minstr cx
       @@ Mov { s = size; dst = Operand.Reg r11; src = Cx.apply_vreg cx index };
       Cx.add_minstr cx
       @@ Lea { s = size; dst = r11; src = Address.index_scale r11 scale };
       Cx.add_minstr cx
       @@ MInstr.Add { s = size; dst = stack_slot; src1 = stack_slot; src2 = Reg r11 };
       ());
    Some stack_slot_address
  | Address.Imm _ -> None
;; *)

(* let apply_allocation_operand ~cx size operand =
  let r11 = Cx.r11 cx size in
  match operand with
  | Operand.Reg reg when Cx.is_spilled cx reg -> Operand.stack_local reg.name
  | Operand.Mem address
    when Address.iter_regs address |> F.Iter.find ~f:(Cx.is_spilled cx) ->
    let stack_slot = Cx.fresh_name cx "spill_address" |> Address.stack_local in
    let address_of_address =
      apply_allocation_address ~cx size address |> Option.value_exn
    in
    Cx.add_minstr cx
    @@ Mov { s = size; dst = Mem stack_slot; src = Mem address_of_address };
    Mem stack_slot
  | Operand.Mem address -> todo [%here]
  | Operand.Reg _ | Operand.Imm _ -> operand
;; *)

type thing =
  { first : int
  ; second : int
  }

module Testing (T : sig
    type t =
      { first : int
      ; second : int
      }
  end) =
struct
  type t = T.t

  let id (x : t) = x
end

(* let testing () =
  let x = M1.id { first = 1; second = 2 } in
  ()
;;

module M2 = Testing (struct
    type t =
      { first : int
      ; second : int
      }
  end) *)

module Empty1 = struct end
module Empty2 = struct end

module Test1 = struct
  module Another (M : sig end) () = struct
    type t =
      { first : int
      ; second : int
      }
  end

  module M1 = Another (Empty1) ()
  module M2 = Another (Empty2) ()
  module M3 = Another (Empty1) ()
end

(* module Test1 = struct
   module Existential (M : sig end) : sig
   type t
   end = struct
   type t = int
   end

   module type Another = sig
   type t = Existential(Empty1).t
   end
   module M1 = Existential (Empty1)
   module M2 = Existential (Empty2)
   module M3 = Existential (Empty1)

   (* let f (x : M1.t) =
   let (y : M1.t) = x in
   let (z : M3.t) = y in
   ()
   ;; *)
   end *)

type 'a vlist =
  [ `Nil
  | `Cons of 'a * 'a vlist
  ]

let open_vlist l = (l : 'a vlist :> [> 'a vlist ])

type another = [ `Another ]

let another : another = `Another
let open_another another = (another : another :> [> another ] as 'a)
let open_ref_another another = (another : another list :> [> another ] list)
