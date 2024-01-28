open! Core
module F = Folds

type 'v graph_fold = ('v, 'v) F.Fold.t

type 'n t =
  { succs : 'n graph_fold
  ; all_nodes : 'n Iter.t
  }

type 'n double =
  { succs : 'n graph_fold
  ; preds : 'n graph_fold
  ; all_nodes : 'n Iter.t
  }
