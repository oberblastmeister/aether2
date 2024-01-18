open! O

type 'v graph_fold = ('v, 'v) F.Fold.t

module type Node = sig
  type t [@@deriving equal, compare, hash, sexp]

  include Comparator.S with type t := t
end

type 'n node = (module Node with type t = 'n)

type 'n t =
  { succs : 'n graph_fold
  ; all_nodes : 'n Iter.t
  }

type 'n double =
  { succs : 'n graph_fold
  ; preds : 'n graph_fold
  ; all_nodes : 'n Iter.t
  }
