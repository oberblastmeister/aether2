open O
open Types

(* type context = { buffer : Buffer.t }

module Cx = struct
  let add cx s = Buffer.add_string cx.buffer s
  let space cx = add cx " "
end

let string_of_size (s : Size.t) =
  match s with
  | Q -> "q"
  | L -> "l"
  | W -> "w"
  | B -> "b"
;;

let print_size cx s = Cx.add cx @@ string_of_size s

let string_of_mach_reg (s : Size.t) (reg : Mach_reg.t) =
  match s with
  | Q ->
    (match reg with
     | RAX -> "rax"
     | RCX -> "rcx"
     | RDX -> "rdx"
     | RBX -> "rbx"
     | RSP -> "rsp"
     | RBP -> "rbp"
     | RSI -> "rsi"
     | RDI -> "rdi"
     | R8 -> "r8"
     | R9 -> "r9"
     | R10 -> "r10"
     | R11 -> "r11"
     | R12 -> "r12"
     | R13 -> "r13"
     | R14 -> "r14"
     | R15 -> "r15")
  | _ -> todo ()
;;

let print_mreg cx (mreg : MReg.t) = Cx.add cx @@ string_of_mach_reg mreg.s mreg.reg

let print_imm cx imm =
  match imm with
  | Imm.Int i -> Cx.add cx @@ string_of_int @@ Int.of_int32_exn i
  | _ -> todo ()
;;

let string_of_scale = function
  | Address.Scale.One -> "1"
  | Two -> "2"
  | Four -> "4"
  | Eight -> "8"
;;

let print_address cx (address : _ Address.t) =
  match address with
  | Imm { offset; scale } ->
    print_imm cx offset;
    Cx.add cx " * ";
    Cx.add cx @@ string_of_scale scale
  | Complex { base; index; offset } ->
    print_imm cx offset;
    (match base with
     | None -> ()
     | Rip -> todo ()
     | Reg r ->
       Cx.add cx " + ";
       print_mreg cx r;
       ());
    (match index with
     | None -> ()
     | Some { index; scale } ->
       Cx.add cx " + ";
       print_mreg cx index;
       Cx.add cx " * ";
       Cx.add cx @@ string_of_scale scale)
;;

let print_operand cx (operand : _ Operand.t) =
  match operand with
  | Imm (Int i) -> Cx.add cx @@ string_of_int @@ Int.of_int32_exn i
  | Imm _ -> todo ()
  | Reg r -> print_mreg cx r
  | Mem mem ->
    Cx.add cx "[";
    print_address cx mem.addr;
    Cx.add cx "]"
;;

let print_operands cx operands =
  List1.of_list operands
  |> Option.iter ~f:(function List1.T (x, xs) ->
    print_operand cx x;
    List.iter xs ~f:(fun o ->
      Cx.add cx ", ";
      print_operand cx o;
      ());
    ())
;;

let print_instr cx name s =
  Cx.add cx "add";
  print_size cx s;
  Cx.space cx;
  ()
;;

let print_minstr cx (instr : MReg.t Instr.t) =
  match instr with
  | NoOp -> ()
  | Add {  dst; src2; _ } ->
    print_instr cx "add" s;
    print_operands cx [ dst; src2 ]
  | Mov { s; dst; src } ->
    print_instr cx "mov" s;
    print_operands cx [ dst; src ]
  | MovAbs { dst; imm } -> ()
  | _ -> raise_s [%message "could print instr" (instr : MReg.t Instr.t)]
;;

(* print_mreg dst;
   print_mreg src2; *)

(* Cx.add cx *)

let print _ = todo () *)
