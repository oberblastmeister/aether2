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
  (Cfg.Graph.to_iteri fn.graph) (fun (label, block) ->
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

module Stack_layout : sig
  type t [@@deriving sexp_of]

  val create
    :  vregs:(Name.t, VReg.t) Entity.Map.t
    -> allocation:Ra.Allocation.t
    -> VReg.t Function.t
    -> t

  val end_offset : t -> int32 -> int32
  val local_offset : t -> Name.t -> int32
  val spilled_offset : t -> Name.t -> int32
end = struct
  type t =
    { stack_size : int32
    ; offset_of_local : (Name.t, int32) Hashtbl.t
    ; locals_offset : int32
    ; end_offset : int32
    ; spilled_offset : int32
    ; offset_of_spilled : (Name.t, int32) Hashtbl.t
    }
  [@@deriving sexp_of]

  open Int32

  let end_offset t i = t.end_offset + i
  let local_offset t name = t.locals_offset + Hashtbl.find_exn t.offset_of_local name
  let spilled_offset t name = t.spilled_offset + Hashtbl.find_exn t.offset_of_spilled name

  let create ~vregs ~allocation fn =
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
    |> F.Iter.iter ~f:(fun spilled_name ->
      let vreg = NameMap.find_exn vregs spilled_name in
      let size =
        Size.to_byte_size vreg.VReg.s |> of_int_exn |> round_up ~to_multiple_of:8l
      in
      Hashtbl.add_exn offset_of_spilled ~key:spilled_name ~data:!spilled_size;
      spilled_size := !spilled_size + size);
    let align size =
      assert (size % 8l = 0l);
      if size % 16l = 8l then size + 8l else size
    in
    (* TODO: align stack, align stuff *)
    { stack_size = align @@ (!locals_size + !spilled_size + !end_size)
    ; end_offset = 0l
    ; locals_offset = !end_size
    ; offset_of_local
    ; spilled_offset = !locals_size + !end_size
    ; offset_of_spilled
    }
  ;;
end

type context =
  { allocation : Ra.Allocation.t
  ; instrs : (Mach_reg.t Instr.t, read_write) Vec.t
  ; stack_layout : Stack_layout.t
  }

(* precondition, mov must be legalized*)
let lower_stack_off ~cx off =
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
;;

(* if can_use_scratch then (

   ) else () *)
let lower_mov ~cx ~can_use_scratch ~dst ~src = ()

(* let apply_allocation_vinstr ~cx instr =
  let open VInstr in
  let open MInstr in
  match instr with
  | Par_mov movs ->
    let convert movs =
      let movs =
        List.map movs ~f:(fun (dst, src) -> Compiler.Windmills.Move.create ~dst ~src)
        |> Array.of_list
      in
      Compiler.Windmills.convert
        ~move:(fun ~dst ~src -> Either.First dst, Either.First src)
        ~get_name:VReg.to_name
        ~scratch:(fun reg ->
          (* this is not valid ssa *)
          Either.Left { reg with precolored = Some R11 })
        movs
      |> Tuple2.map_fst ~f:Vec.to_list
    in
    let instrs, did_use_scratch = convert movs in
    let open Reg_alloc.Alloc_reg in
    (* let maybe_spill ~dst ~src =
      (match
         Ra.Allocation.(
           find_exn cx.allocation dst.VReg.name, find_exn cx.allocation src.VReg.name)
       with
       | Spilled, Spilled when did_use_scratch ->
         Vec.push cx.instrs (Push { src = Reg.mach_reg reg.s Mach_reg.R11 });
         ()
       | Spilled, _ | _, Spilled -> ()
       | InReg dst_reg, InReg src_reg -> ());
      ()
    in *)
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
;; *)

let apply_allocation_block ~cx ~allocation (block : _ Block.t) =
  let instrs = Vec.create () in
  (Block.instrs_forward_fold block) (fun instr -> ());
  ()
;;

let map_last vec ~f =
  let vec = Vec.copy_exact ~size:(Vec.length vec + 1) vec in
  Vec.set vec (Vec.length vec - 1) (f (Vec.get vec (Vec.length vec - 1)));
  Vec.freeze vec
;;

let remove_ssa (fn : VReg.t Function.t) =
  let graph = fn.graph in
  let graph =
    Cfg.Graph.foldi graph ~init:graph ~f:(fun graph (label, block) ->
      let jump = Block.get_jump block in
      let jump_with_no_args =
        Jump.map_block_calls jump ~f:(fun j -> { j with args = Block_call [] })
      in
      let block_calls = Jump.block_calls_fold jump |> F.Iter.to_list in
      let get_data (j : _ Block_call.t) =
        let args = Maybe_block_call.get_args j.args in
        let to_block = Cfg.Graph.find_exn j.label graph in
        let params = to_block |> Block.get_block_args in
        let par_mov = List.zip_exn params args |> VInstr.Par_mov in
        to_block, par_mov
      in
      match block_calls with
      | [ j ] ->
        let to_block, par_mov = get_data j in
        let instrs =
          block.instrs |> fun vec -> Vec.copy_exact ~size:(Vec.length vec + 1) vec
        in
        let _ = Vec.pop_exn instrs in
        Vec.push instrs @@ Instr.Virt par_mov;
        Vec.push instrs @@ Jump jump_with_no_args;
        let instrs = Vec.freeze instrs in
        graph
        |> Cfg.Graph.set label { Block.instrs }
        |> Cfg.Graph.set j.label (Block.replace_first (Real NoOp) to_block)
      | js ->
        List.fold js ~init:graph ~f:(fun graph j ->
          let to_block, par_mov = get_data j in
          let block =
            { Block.instrs = block.instrs |> map_last ~f:(fun _ -> Jump jump_with_no_args)
            }
          in
          graph
          |> Cfg.Graph.set label block
          |> Cfg.Graph.set label (Block.replace_first (Virt par_mov) to_block)))
  in
  { Function.graph }
;;
