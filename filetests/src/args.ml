open Core
open Cmdliner

type common = { debug : bool } [@@deriving sexp_of]

type t =
  | Compile of
      { files : string list
      ; quiet : bool
      ; common : common
      }
  | Test of
      { common : common
      ; options : unit
      }
[@@deriving sexp_of]

module Syntax = struct
  let pure = Term.const
  let ( <$> ) f x = Term.app (pure f) x
  let ( <*> ) = Cmdliner.Term.app
  let ( let+ ) x f = f <$> x
  let ( and+ ) x y = Term.(const (fun x y -> x, y) $ x $ y)
end

open Syntax

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
  and+ quiet = Arg.(value & flag & info [ "q"; "quiet" ] ~doc:"Suppress all output") in
  Compile { common; files; quiet }
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
