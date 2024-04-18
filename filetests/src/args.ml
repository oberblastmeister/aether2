open Core
open Cmdliner

type common = { debug : bool } [@@deriving sexp_of]

type t =
  | Compile of
      { files : string list
      ; quiet : bool
      ; emit : Aether2.Lir.Driver.emit option
      ; common : common
      }
  | Test of
      { common : common
      ; options : unit
      }
[@@deriving sexp_of]

type emit = Aether2.Lir.Driver.emit

module Syntax = struct
  let pure = Term.const
  let ( <$> ) f x = Term.app (pure f) x
  let ( <*> ) = Cmdliner.Term.app
  let ( let+ ) x f = f <$> x
  let ( and+ ) x y = Term.(const (fun x y -> x, y) $ x $ y)
end

open Syntax

let emit_conv =
  Arg.conv
    ( (function
        | "lir" -> Ok (Some (Lir : emit))
        | "tir" -> Ok (Some (Tir : emit))
        | "x86" -> Ok (Some X86)
        | "asm" -> Ok (Some Asm)
        | _ -> Error (`Msg "invalid emit"))
    , fun fmt emit ->
        match emit with
        | None -> Format.fprintf fmt "none"
        | Some emit ->
          Format.fprintf fmt "%s"
          @@ Sexp.to_string_hum
          @@ Aether2.Lir.Driver.sexp_of_emit emit )
;;

let sdocs = Manpage.s_common_options

let common_term =
  let+ debug =
    let doc = "turn on debug logging" in
    Arg.(value & flag & info [ "debug" ] ~doc)
  in
  { debug }
;;

let compile_term =
  let+ common = common_term
  and+ files = Arg.(value & (pos_all file) [] & info [] ~docv:"FILE or DIR")
  and+ quiet = Arg.(value & flag & info [ "q"; "quiet" ] ~doc:"Suppress all output")
  and+ emit =
    Arg.(value & opt ~vopt:(Some (Asm : emit)) emit_conv None & info [ "emit" ])
  in
  Compile { common; files; quiet; emit }
;;

let compile_cmd =
  let doc = "compile lir" in
  let info = Cmd.info "compile" ~doc ~sdocs in
  Cmd.v info compile_term
;;

let test_term =
  let+ common = common_term
  and+ options = pure () in
  Test { common; options }
;;

let test_cmd =
  let doc = "run filetests" in
  let info = Cmd.info "test" ~doc ~sdocs in
  Cmd.v info test_term
;;

let main_cmd =
  let info = Cmd.info "lirc" ~doc:"lir compiler" in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [ compile_cmd; test_cmd ]
;;

let main () = Cmd.eval_value main_cmd
