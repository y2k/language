let () =
  let shared = In_channel.(with_open_bin "prelude/data/shared.clj" input_all) in
  let result =
    [ "prelude_java_macro"; "prelude_eval_macro"; "prelude_eval"; "prelude_js_macro" ]
    |> List.map (fun lang ->
           let filename = "prelude/data/" ^ lang ^ ".clj" in
           let code = shared ^ "\n" ^ In_channel.(with_open_bin filename input_all) in
           "let " ^ lang ^ " = {|\n" ^ code ^ "\n|}\n")
    |> List.append
         [
           "(* THIS FILE IS GENERATED *)\n";
           (* "let java_runtime = {|\n" ^ In_channel.(with_open_bin "prelude/data/y2k/RT.java" input_all) ^ "\n|}\n"; *)
         ]
    |> String.concat "\n"
  in
  Out_channel.(with_open_bin "core/prelude.ml" (fun o -> output_string o result))
