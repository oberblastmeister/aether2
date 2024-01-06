open Ppxlib

(* let extension =
   Extension.declare
   "my_extension"
   some_context
   some_pattern
   expand_function *)
let extension =
  Extension.declare
    "my_extension"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (failwith "")
;;

let rule = Context_free.Rule.extension extension
let () = Driver.register_transformation ~rules:[ rule ] "my_transformation"
