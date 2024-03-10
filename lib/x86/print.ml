open O
open Types

type context = Buffer.t

module Cx = struct
  let add cx s = Buffer.add_string cx s
  let space cx = add cx " "
end

let suffix_of_size (s : Size.t) =
  match s with
  | Q -> "q"
;;

let print_pointer_size (s : Size.t) =
  match s with
  | Q -> "qword ptr"
;;

let print_size cx s = Cx.add cx @@ suffix_of_size s

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
;;

let string_of_cond (cond : Cond.t) =
  match cond with
  | E -> "e"
  | NE -> "ne"
  | B -> "b"
  | BE -> "be"
  | A -> "a"
;;

let print_mreg cx (mreg : MReg.t) = Cx.add cx @@ string_of_mach_reg mreg.s mreg.reg

let print_imm cx imm =
  match imm with
  | Imm.Int i -> Cx.add cx @@ string_of_int @@ Int.of_int32_exn i
  | imm -> raise_s [%message "could not print imm" (imm : Imm.t)]
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
     | Rip -> todo [%here]
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

let print_operand b (operand : _ Operand.t) =
  match operand with
  | Imm (Int i) -> Cx.add b @@ string_of_int @@ Int.of_int32_exn i
  | Imm _ -> todo [%here]
  | Reg r -> print_mreg b r
  | Mem mem ->
    bprintf b "%s [%a]" (print_pointer_size mem.size) print_address mem.addr;
    ()
;;

(* Cx.add cx "[";
    print_address cx mem.addr;
    Cx.add cx "]" *)

(* need to add dword ptr and qword ptr stuff *)
(* todo [%here] *)

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

let op = print_operand

let suffix (op : _ Flat.Op.t) =
  match op with
  | Imm imm -> raise_s [%message "no suffix for immediate" (imm : Imm.t)]
  | Mem { size; _ } -> suffix_of_size size
  | Reg { MReg.s; _ } -> suffix_of_size s
;;

let i1 b s x = bprintf b "\t%s\t%a" s op x
let i2_s cx s x y = bprintf cx "\t%s\t%a, %a" s op x op y

let print_instr b (instr : MReg.t Flat.Instr.t) =
  match instr with
  | Add { dst; src } -> i2_s b "add" dst src
  | Sub { dst; src } -> i2_s b "sub" dst src
  | Mov { dst; src } -> i2_s b "mov" dst src
  | Cmp { src1; src2 } -> i2_s b "cmp" src1 src2
  | Test { src1; src2 } -> i2_s b "test" src1 src2
  | Set { cond; dst } -> i1 b ("set" ^ string_of_cond cond) dst
  | J { cond; src } -> bprintf b "\t%s %s" ("j" ^ string_of_cond cond) src
  | Jmp { src } -> bprintf b "\tjmp %s" src
  | MovAbs { dst; imm } ->
    bprintf
      b
      "\t%s\t%a, %a"
      "movabs"
      op
      dst
      (fun b i -> Buffer.add_string b (Int64.to_string_hum i))
      imm
  | Ret -> bprintf b "\tret"
  | Call { src } -> bprintf b "\tcall\t%s" src
  | _ -> raise_s [%message "could not print instr" (instr : MReg.t Flat.Instr.t) [%here]]
;;

let print_line b line =
  (match line with
   | Flat.Line.Instr instr -> print_instr b instr
   | SectionText -> bprintf b "\t.text"
   | Type (s, ty) -> bprintf b "\t.type\t%s,%s" s ty
   | Label l -> bprintf b "%s:" l
   | Comment s -> bprintf b "# %s" s
   | Global s -> bprintf b "\t.globl\t%s" s);
  Buffer.add_char b '\n'
;;

let run program =
  let buffer = Buffer.create 1000 in
  bprintf buffer "\t.intel_syntax\tnoprefix\n";
  Vec.iter program ~f:(fun line -> print_line buffer line);
  Buffer.contents buffer
;;
