open O
open Ast

type context = Buffer.t

module Cx = struct
  let add cx s = Buffer.add_string cx s
  let space cx = add cx " "
end

let suffix_of_size (s : Size.t) =
  match s with
  | Q -> "q"
  | B -> "b"
;;

let prefix_of_size (s : Size.t) =
  match s with
  | Q -> "qword ptr"
  | B -> "byte ptr"
;;

let print_size cx s = Cx.add cx @@ suffix_of_size s

let string_of_mach_reg8l = function
  | Mach_reg.RAX -> "al"
  | RBX -> "bl"
  | RCX -> "cl"
  | RDX -> "dl"
  | RSP -> "spl"
  | RBP -> "bpl"
  | RSI -> "sil"
  | RDI -> "dil"
  | R8 -> "r8b"
  | R9 -> "r9b"
  | R10 -> "r10b"
  | R11 -> "r11b"
  | R12 -> "r12b"
  | R13 -> "r13b"
  | R14 -> "r14b"
  | R15 -> "r15b"
;;

let string_of_mach_reg64 = function
  | Mach_reg.RAX -> "rax"
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
  | R15 -> "r15"
;;

let string_of_mach_reg = function
  | Size.Q -> string_of_mach_reg64
  | B -> string_of_mach_reg8l
;;

let string_of_cond (cond : Cond.t) =
  match cond with
  | E -> "e"
  | NE -> "ne"
  | B -> "b"
  | BE -> "be"
  | A -> "a"
  | AE -> "ae"
  | G -> "g"
  | GE -> "ge"
  | L -> "l"
  | LE -> "le"
;;

let print_mreg cx s (mreg : MReg.t) = Cx.add cx @@ string_of_mach_reg s mreg.reg

let print_imm cx imm =
  match imm with
  | Imm.Int i -> Cx.add cx @@ Imm_int.to_string i
  | Imm.Stack _ -> raise_s [%message "could not print stack imm" (imm : Imm.t)]
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
     | Rsp ->
       Cx.add cx " + ";
       print_mreg cx Q (MReg.create RSP);
       ()
     | Rip -> todo [%here]
     | Reg r ->
       Cx.add cx " + ";
       print_mreg cx Q r;
       ());
    (match index with
     | None -> ()
     | Some { index; scale } ->
       Cx.add cx " + ";
       print_mreg cx Q index;
       Cx.add cx " * ";
       Cx.add cx @@ string_of_scale scale)
;;

let print_operand s b (operand : _ Operand.t) =
  match operand with
  | Imm (Int i) -> Cx.add b @@ Imm_int.to_string i
  | Imm _ -> todo [%here]
  | Reg r -> print_mreg b s r
  | Mem addr ->
    bprintf b "%s [%a]" (prefix_of_size s) print_address addr;
    ()
;;

let op = print_operand
let i0 b s = bprintf b "\t%s" s
let i1 size b s x = bprintf b "\t%s\t%a" s (op size) x
let i2_s size cx s x y = bprintf cx "\t%s\t%a, %a" s (op size) x (op size) y

let print_instr b (instr : MReg.t Flat.Instr.t) =
  match instr with
  | Add { s; dst; src } -> i2_s s b "add" dst src
  | Sub { s; dst; src } -> i2_s s b "sub" dst src
  | Mov { s; dst; src } -> i2_s s b "mov" dst src
  | Cmp { s; src1; src2 } -> i2_s s b "cmp" src1 src2
  | Test { s; src1; src2 } -> i2_s s b "test" src1 src2
  | Set { cond; dst } -> i1 B b ("set" ^ string_of_cond cond) dst
  | J { cond; src } -> bprintf b "\t%s %s" ("j" ^ string_of_cond cond) src
  | Jmp { src } -> bprintf b "\tjmp %s" src
  | MovAbs { dst; imm } ->
    bprintf
      b
      "\t%s\t%a, %a"
      "movabs"
      (op Q)
      dst
      (fun b i -> Buffer.add_string b (Z.to_string_hum i))
      imm
  | Ret -> i0 b "ret"
  | Call { src } -> bprintf b "\tcall\t%s" src
  | MovZx { dst_size; src_size; dst; src } ->
    bprintf b "\tmovzx\t%a, %a" (op dst_size) dst (op src_size) src
  | Imul { s; dst; src } -> i2_s s b "imul" dst src
  | Lea { s; dst; src } -> i2_s s b "lea" dst src
  | Push { s; src } -> i1 s b "push" src
  | Pop { s; dst } -> i1 s b "pop" dst
  | Div { s; src } -> i1 s b "div" src
  | Idiv { s; src } -> i1 s b "idiv" src
  | Cqto -> i0 b "cqo"
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
