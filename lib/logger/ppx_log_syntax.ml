open! Core
(* module M = Ppx_log_types.Message_data
module M = Ppx_log_types.Message_source*)

open struct
  let string_of_level = function
    | `Debug -> "DEBUG"
    | `Info -> "INFO"
    | `Error -> "ERROR"
  ;;
end

type time = Nothing.t
type t = Log.t
type return_type = unit

let would_log log level = true
let default = ()
let sexp ?level ?pos log sexp = ()

module Global = struct
  type return_type = unit

  let default = ()
  let would_log level = !Log.would_log

  let sexp ?level ?pos sexp =
    print_s
      Sexp.(
        List
          (List.concat
             [ Option.map ~f:(fun level -> Atom (string_of_level level)) level
               |> Option.to_list
             ; Option.map ~f:(fun pos -> Atom (Source_code_position.to_string pos)) pos
               |> Option.to_list
             ; [ sexp ]
             ]));
  ;;
end
