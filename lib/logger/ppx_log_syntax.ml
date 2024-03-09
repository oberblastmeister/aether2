open Core

let string_of_level = function
  | `Debug -> "DEBUG"
  | `Info -> "INFO"
  | `Error -> "ERROR"
;;

module T = struct
  type t = Log.t
  type return_type = unit

  let would_log log level = true
  let sexp ?level ?pos log sexp = ()
  let default = ()
end

include T

module Global = struct
  type return_type = unit

  let default = ()
  let would_log level = false

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
             ]))
  ;;
end
