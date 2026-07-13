let () =
  let target = ref "eval" in
  Arg.parse
    [ ("--target", Arg.Set_string target, "TARGET output target: eval, js, or java") ]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg)))
    "language [--target eval|js|java]";
  match Language_main.Runner.run ~target:!target (In_channel.input_all stdin) with
  | Ok output -> print_endline output
  | Error message ->
      prerr_endline message;
      exit 1
