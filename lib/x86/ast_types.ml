(* all register types must have sizes because they may be used inside addresses *)

(*
   module GOperand = struct
   type imm = Imm_
   type reg = Reg_
   type mem = Mem_

   type ('r, 'op) t =
   | Imm : Imm.t -> ('r, imm) t
   | Reg : 'r -> ('r, reg) t
   | Mem : 'r Address.t -> ('r, mem) t

   let to_operand (type op) (t : (_, op) t) =
   match t with
   | Imm imm -> Operand.Imm imm
   | Reg reg -> Reg reg
   | Mem addr -> Mem addr
   ;;

   let mem_val = function
   | Mem addr -> addr
   ;;

   let imm_val = function
   | Imm imm -> imm
   ;;

   let reg_val = function
   | Reg reg -> reg
   ;;
   end *)
