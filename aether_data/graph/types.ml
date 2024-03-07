open! Core
module F = Folds

type 'v graph_iter = ('v, 'v) F.Fold.t

type 'n t =
  { succs : 'n graph_iter
  ; all_nodes : 'n F.Iter.t
  }

type 'n double =
  { succs : 'n graph_iter
  ; preds : 'n graph_iter
  ; all_nodes : 'n F.Iter.t
  }
