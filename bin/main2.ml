open Lib__.Common

let main () =
  let command = ref "" in
  let target = ref "" in
  let src = ref "" in
  let capture_stdin = ref false in
  let namespace = ref "" in
  let root_dir = ref "" in
  Arg.parse
    [
      ("-target", Arg.Set_string target, "Target: js, java, eval, bytecode");
      ("-src", Arg.Set_string src, "Source file (use :stdin for standard input)");
      ("-capture_stdin", Arg.Bool (( := ) capture_stdin), "Capute stdin");
      ("-namespace", Arg.Set_string namespace, "Namespace");
      ("-root", Arg.Set_string root_dir, "Root directory");
    ]
    (( := ) command) "ly2k";
  let code = In_channel.(with_open_bin !src input_all) in
  match !target with
  | "java" -> Core.compile !namespace false !src !root_dir code |> print_endline
  | "eval" | "repl" ->
      FileReader.with_scope
        (fun () ->
          let input = if !capture_stdin then In_channel.(with_open_bin !src input_all) else "" in
          Core.eval !src input code |> print_endline)
        ()
  | t -> failwith @@ "Invalid target " ^ t
