let main () =
  let command = ref "" in
  let target = ref "" in
  let src = ref "" in
  Arg.parse
    [
      ("-target", Arg.Set_string target, "Target: js, java, eval, bytecode");
      ("-src", Arg.Set_string src, "Source file (use :stdin for standard input)");
    ]
    (( := ) command) "ly2k";
  let code = In_channel.(with_open_bin !src input_all) in
  match !target with
  | "java" -> Core.compile false !src (Sys.getcwd ()) code |> print_endline
  | "eval" -> Core.eval (In_channel.input_all stdin) code |> print_endline
  | t -> failwith @@ "Invalid target " ^ t
