open Core

include
  Ppx_log_types.S
  with type t = Log.t
   and type return_type = unit
   and type Global.return_type = unit
