open O

module Control = struct
  type e = E__
  type o = O__
  type c = C__
end

module SControl = struct
  type 'a t = SO : Control.o t | SC : Control.c t | SE : Control.e t
end

module Label = struct
  module T = struct
    type t = T of string [@@deriving show, equal, compare, sexp, hash]
  end

  include T
  module Hashtbl = Hashtbl.Make (T)
  module Map = Map.Make (T)
end

module Name = struct
  module T = struct
    type t = Name of string | GenName of (string * int)
    [@@deriving sexp, equal, compare, hash]
  end

  include T
  module Hashtbl = Hashtbl.Make (T)
  module Map = Map.Make (T)
end

module Common = struct
  module type S = sig
    type t

    include Sexpable.S with type t := t
    include Comparable.S with type t := t
    include Equal.S with type t := t
  end
end

module SomeInstr = struct
  type 'i t = T : ('c SControl.t * ('c -> 'i) Higher_kinded.t) -> 'i t
end

module type Instr = sig
  open Control

  type 'c t

  include Higher_kinded.S with type 'c t := 'c t

  val jumps : c t -> Label.t list
  val t_of_sexp : Sexp.t -> higher_kinded SomeInstr.t
  val ot_of_sexp : Sexp.t -> o t
  val ct_of_sexp : Sexp.t -> c t
  val et_of_sexp : Sexp.t -> e t
  val sexp_of_t : 'c t -> Sexp.t
  val compare : 'c t -> 'd t -> int
  val equal : 'c t -> 'd t -> bool
end

(* module Block = struct
     type ('et, 'ot, 'ct) t = { entry : 'et; body : 'ot list; exit : 'ct }
     [@@deriving accessors, sexp]
   end *)

(* module type GRAPH = sig
     type et
     type ot
     type ct
     type block

     type t = {
       entry : Label.t;
       body : (Label.t, block) Hashtbl.t;
       exit : Label.t;
     }
     [@@deriving accessors, sexp]
   end *)

module type BLOCK = functor (I : Instr) -> sig
  open Control

  type t = { entry : e I.t; body : o I.t list; exit : c I.t }
  [@@deriving accessors]

  type 'c i = 'c I.t

  val sexp_of_t : t -> Sexp.t
  val equal : t -> t -> bool
end

module type BlockLike = sig
  type t
  type instr

  val get_instrs : t -> instr list
end

module MakeBlock : BLOCK =
functor
  (I : Instr)
  ->
  struct
    open Control

    type t = { entry : e I.t; body : o I.t list; exit : c I.t }
    [@@deriving accessors]

    type 'c i = 'c I.t

    let sexp_of_t { entry; body; exit } =
      Sexp.List
        [
          Sexp.List [ Sexp.Atom "entry"; I.sexp_of_t entry ];
          Sexp.List
            [ Sexp.Atom "body"; Sexp.List (List.map ~f:I.sexp_of_t body) ];
          Sexp.List [ Sexp.Atom "exit"; I.sexp_of_t exit ];
        ]

    let equal b1 b2 =
      I.equal b1.entry b2.entry && I.equal b1.exit b2.exit
      && List.equal I.equal b1.body b2.body
  end

module Graph = struct
  type 'b t = { entry : Label.t; body : 'b Label.Map.t; exit : Label.t }
  [@@deriving accessors, sexp_of]
end

let%expect_test "testing" =
  printf "%d" (1 + 2);
  [%expect {|3|}]

let%test "testing" = [%equal: Label.t] (Label.T "a") (Label.T "a")
