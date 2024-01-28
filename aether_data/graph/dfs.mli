open! Core
open Types

type 'n event =
  | Cycle of 'n
  | Enter of 'n
  | Exit of 'n

val visit
  :  f:('n event -> unit)
  -> start:'n list
  -> set:'n Constructors.some_set
  -> 'n t
  -> unit

val visit_preorder
  :  f:('a -> unit)
  -> start:'a list
  -> set:'a Constructors.some_set
  -> 'a t
  -> unit

val visit_postorder
  :  f:('a -> unit)
  -> start:'a list
  -> set:'a Constructors.some_set
  -> 'a t
  -> unit

val postorder
  :  start:'a list
  -> set:'a Constructors.some_set
  -> 'a t
  -> ('a, Perms.Read_write.t) Vec.t

val reverse_postorder
  :  start:'a list
  -> set:'a Constructors.some_set
  -> 'a t
  -> ('a, Perms.Read_write.t) Vec.t

val preorder
  :  start:'a list
  -> set:'a Constructors.some_set
  -> 'a t
  -> ('a, Perms.Read_write.t) Vec.t
