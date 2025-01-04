let () =
  let shared = In_channel.(with_open_bin "prelude/data/shared.clj" input_all) in
  let result =
    [ "js"; "java"; "bytecode"; "interpreter" ]
    |> List.map (fun lang ->
           let filename = "prelude/data/" ^ lang ^ ".clj" in
           let code = shared ^ "\n" ^ In_channel.(with_open_bin filename input_all) in
           "let " ^ lang ^ " = {|\n" ^ code ^ "\n|}\n")
    |> List.append
         [
           "(* THIS FILE IS GENERATED *)\n";
           "let java_runtime = {|\n" ^ In_channel.(with_open_bin "prelude/data/y2k/RT.java" input_all) ^ "\n|}\n";
         ]
    |> String.concat "\n"
  in
  Out_channel.(with_open_bin "lib/preludes.ml" (fun o -> output_string o result))
