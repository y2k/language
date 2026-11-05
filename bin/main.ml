let () =
  let target = ref "eval" in
  let source_map = ref false in
  let source_file = ref None in
  let unpack_fex = ref false in
  let output_dir = ref None in
  Arg.parse
    [
      ("--target", Arg.Set_string target, "TARGET output target: eval, js, java, or fex");
      ("--unpack-fex", Arg.Set unpack_fex, "Unpack a FEX bundle from stdin");
      ("--output-dir", Arg.String (fun path -> output_dir := Some path), "PATH directory for unpacked FEX files");
      ("--source-map", Arg.Set source_map, "Embed an inline source map (js only)");
      ("--source-file", Arg.String (fun path -> source_file := Some path), "PATH source map name (default: user.clj)");
    ]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg)))
    "language [--target eval|js|java|fex] [--source-map] [--source-file PATH]";
  if !unpack_fex then (
    let result =
      match !output_dir with
      | None -> Error "--unpack-fex requires --output-dir"
      | Some output_dir -> Fex_unpack.run ~output_dir stdin
    in
    match result with
    | Ok () -> ()
    | Error message ->
        prerr_endline message;
        exit 1)
  else
    match
      Language_main.Runner.run ~source_map:!source_map ?source_file:!source_file ~target:!target
        (In_channel.input_all stdin)
    with
    | Ok output -> if !target = "fex" then print_string output else print_endline output
    | Error message ->
        prerr_endline message;
        exit 1
