open O
open Types
open Utils.Instr_types
module Interference = Compiler.Reg_alloc.Interference
module Reg_alloc = Compiler.Reg_alloc

module Ra = Reg_alloc.Make (struct
    module Register = struct
      include Mach_reg

      let order =
        [ (* callee saved *)
          RAX
        ; RDI
        ; RSI
        ; RDX
        ; RCX
        ; R8
        ; R9
        ; R10
        ; (* 11 used for scratch register *)
          (* caller saved *)
          RBX
        ; R12
        ; R13
        ; R14
        ; R15
        ]
      ;;
    end

    module RegisterSet = struct
      type t = Register.t Hash_set.t [@@deriving sexp_of]
      type value = Register.t

      let create () = Hash_set.create (module Register)
      let mem = Hash_set.mem
      let add = Hash_set.add
    end
  end)

module NameMap = Entity.Map.Make (Name)

let transfer = Dataflow.Liveness.instr_transfer |> Cfg.Dataflow.Instr_transfer.transfer

let add_block_edges interference block live_out =
  let live_out = ref live_out in
  Block.instrs_backward_fold block (fun instr ->
    let without =
      match Instr.to_variant instr with
      | Instr_variant.Real (MInstr.Mov { src = Operand.Reg src; _ }) -> [ src ]
      | _ -> []
    in
    let is_edge def live =
      (* don't add an edge to itself *)
      (not ([%equal: VReg.t] def live))
      (* don't add an edge to something that is a register mov *)
      (* we want these to be allocated to the same register, so we can remove the redundant mov *)
      && not (List.mem without live ~equal:[%equal: VReg.t])
    in
    (* make sure we at least add the defs in, because the register allocator uses the domain of interference as all nodes *)
    Instr.defs_fold instr
    |> F.Iter.iter ~f:(fun def -> Interference.add_node interference def.VReg.name);
    (* add interference edges *)
    F.Iter.(
      product (Instr.defs_fold instr) (FC.Set.iter !live_out)
      |> filter ~f:(fun (def, live) -> is_edge def live)
      |> iter ~f:(fun (def, live) ->
        Interference.add_edge interference def.VReg.name live.VReg.name;
        ()));
    live_out := transfer instr !live_out;
    ());
  ()
;;

let collect_precolored fn =
  F.Fold.(
    Function.instrs_forward_fold
    @> Instr.regs_fold
    @> of_fn (fun (vreg : VReg.t) ->
      vreg.precolored |> Option.map ~f:(fun reg -> `name vreg.name, `reg reg))
    @> FC.Option.fold)
    fn
  |> F.Iter.map ~f:(fun (`name name, `reg reg) -> name, reg)
  |> NameMap.of_iter
;;

let construct_fn fn =
  let _, live_out_facts = Dataflow.Liveness.run fn in
  let interference = Interference.create () in
  (FC.Map.foldi fn.graph.blocks) (fun (label, block) ->
    let live_out = Cfg.Dataflow.Fact_base.find_exn live_out_facts label in
    add_block_edges interference block live_out);
  interference
;;

let alloc_fn fn =
  let open Result.Let_syntax in
  let interference = construct_fn fn in
  let precolored = collect_precolored fn in
  let%bind allocation = Ra.run ~precolored ~interference in
  Ok allocation
;;

let collect_all_vregs fn =
  (Function.instrs_forward_fold @> Instr.regs_fold) fn
  |> F.Iter.map ~f:(fun (reg : VReg.t) -> reg.name, reg)
  |> NameMap.of_iter
;;

type context =
  { allocation : Ra.Allocation.t
  ; instrs : (Mach_reg.t Instr.t, read_write) Vec.t
  }

type stack_layout =
  { stack_size : int32
  ; offset_of_local : (Name.t, int32) Hashtbl.t
  ; locals_offset : int32
  ; end_offset : int32
  ; spilled_offset : int32
  ; offset_of_spilled : (Name.t, int32) Hashtbl.t
  }

let calculate_stack_layout ~vregs ~allocation fn =
  let open Int32 in
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
  let offset_of_spilled = Hashtbl.create (module Name) in
  let spilled_size = ref 0l in
  Ra.Allocation.to_spilled_iter allocation
  |> F.Iter.iter ~f:(fun spilled ->
    let vreg = NameMap.find_exn vregs spilled in
    let size =
      Size.to_byte_size vreg.VReg.s |> of_int_exn |> round_up ~to_multiple_of:8l
    in
    Hashtbl.add_exn offset_of_spilled ~key:spilled ~data:!spilled_size;
    spilled_size := !spilled_size + size);
  (* TODO: align stack, align stuff *)
  { stack_size = !locals_size + !spilled_size + !end_size
  ; end_offset = 0l
  ; locals_offset = !end_size
  ; offset_of_local
  ; spilled_offset = !locals_size + !end_size
  ; offset_of_spilled
  }
;;

let apply_allocation_vinstr ~cx instr =
  let open VInstr in
  match instr with
  | Par_mov movs ->
    (* let convert movs =
      Compiler.Windmills.convert
        ~move:(fun ~dst ~src -> `dst dst, `src src)
        ~get_name:VReg.to_name
        ~scratch:(fun reg : MReg.t ->
          let s = reg.s in
          { s; reg = R11 })
        (movs
         |> List.map ~f:(fun (dst, src) -> Compiler.Windmills.Move.create ~dst ~src)
         |> Array.of_list)
    in
    let res, did_use_scratch = convert movs in *)
    (* let spill reg =
       let name =
       in *)
    (* let maybe_spill ~dst src =
      let open Reg_alloc.Alloc_reg in
      match
        Ra.Allocation.(
          ( find_exn cx.allocation (Reg.name_exn dst)
          , find_exn cx.allocation (Reg.name_exn src) ))
      with
      | Spilled, Spilled when did_use_scratch ->
        (* Vec.push cx.instrs (Push { src = Reg.mach_reg reg.s Mach_reg.R11 }); *)
        ()
      | Spilled, _ | _, Spilled -> ()
      | InReg dst_reg, InReg src_reg -> ()
    in *)
    (* Vec.iter res ~f:(fun (`dst dst, `src src) ->
       ();
       ()); *)
    let _f dst src =
      (* let s = dst.Reg.s in
      assert (Size.(equal s src.Reg.s));
      let dst_alloc = Reg_alloc.Allocation.find_exn cx.allocation (Reg.name_exn dst) in
      let src_alloc = Reg_alloc.Allocation.find_exn cx.allocation (Reg.name_exn src) in
      Mov { s; dst; src } *)
      todo ()
    in
    ()
  | _ -> ()
;;

let apply_allocation_block ~cx ~allocation (block : _ Block.t) =
  let instrs = Vec.create () in
  (Block.instrs_forward_fold block) (fun instr -> ());
  ()
;;
