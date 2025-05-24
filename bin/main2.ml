let main () =
  let command = ref "" in
  let target = ref "" in
  let src = ref "" in
  Arg.parse
    [
      ("-target", Arg.Set_string target, "Target: js, java, repl, bytecode");
      ("-src", Arg.Set_string src, "Source file (use @stdin for standard input)");
    ]
    (( := ) command) "ly2k";
  let code = In_channel.(with_open_bin !src input_all) in
  Core.compile true !src (Sys.getcwd ()) code |> print_endline
