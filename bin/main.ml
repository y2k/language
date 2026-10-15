let () =
  let target = ref "eval" in
  let source_map = ref false in
  let source_file = ref None in
  Arg.parse
    [
      ("--target", Arg.Set_string target, "TARGET output target: eval, js, or java");
      ("--source-map", Arg.Set source_map, "Embed an inline source map (js only)");
      ("--source-file", Arg.String (fun path -> source_file := Some path), "PATH source map name (default: user.clj)");
    ]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg)))
    "language [--target eval|js|java] [--source-map] [--source-file PATH]";
  match
    Language_main.Runner.run ~source_map:!source_map ?source_file:!source_file ~target:!target
      (In_channel.input_all stdin)
  with
  | Ok output -> print_endline output
  | Error message ->
      prerr_endline message;
      exit 1
