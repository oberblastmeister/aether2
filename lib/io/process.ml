open Unix

let collect_chan (channel : in_channel) : string =
  let rec loop acc =
    match input_line channel with
    | exception End_of_file -> acc
    | line -> loop (acc ^ line ^ "\n")
  in
  loop ""
;;

let print_chan channel =
  let rec loop () =
    let () = print_endline (input_line channel) in
    loop ()
  in
  try loop () with
  | End_of_file -> close_in channel
;;

let run () =
  let ocaml_stdout, ocaml_stdin, ocaml_stderr =
    Unix.open_process_args_full "ls" [| "ls" |] [||]
  in
  close_out ocaml_stdin;
  print_chan ocaml_stdout;
  print_chan ocaml_stderr;
  print_endline "terminado!"
;;

let run_stdout cmd args =
  let ((stdout, stdin, stderr) as proc) =
    Unix.open_process_args_full cmd (Array.concat [ [| cmd |]; Array.of_list args ]) [||]
  in
  let result = collect_chan stdout in
  let status = Unix.close_process_full proc in
  result
;;
