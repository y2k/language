let generate_java_runtime ~target ~name ~src =
  let ic =
    Unix.open_process_in
      (Printf.sprintf
         "_build/default/bin/main.exe -log false -target %s -namespace y2k \
          -src 'prelude/data/%s'"
         target src)
  in
  let stdout = In_channel.input_all ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> "let " ^ name ^ " = {|\n" ^ stdout ^ "\n|}\n"
  | Unix.WEXITED n | Unix.WSIGNALED n | Unix.WSTOPPED n ->
      prerr_endline stdout;
      failwith ("Process stopped by signal " ^ string_of_int n)

let () =
  let shared = In_channel.(with_open_bin "prelude/data/shared.clj" input_all) in
  let result =
    [
      "prelude_java_v2_macro";
      "prelude_eval_macro";
      "prelude_eval";
      "prelude_js_macro";
    ]
    |> List.map (fun lang ->
        let filename = "prelude/data/" ^ lang ^ ".clj" in
        let code =
          shared ^ "\n" ^ In_channel.(with_open_bin filename input_all)
        in
        "let " ^ lang ^ " = {|\n" ^ code ^ "\n|}\n")
    |> List.cons
         (generate_java_runtime ~target:"java_v2" ~name:"java_runtime2_v2"
            ~src:"prelude_java_v2.clj")
    |> List.append
         [
           "let js_runtime = {|\n"
           ^ In_channel.(with_open_bin "prelude/data/y2k/rt.js" input_all)
           ^ "\n|}\n";
         ]
    |> List.append
         [
           "let java_runtime = {|\n"
           ^ In_channel.(with_open_bin "prelude/data/y2k/RT.java" input_all)
           ^ "\n|}\n";
         ]
    |> List.cons "(* THIS FILE IS GENERATED *)\n"
    |> String.concat "\n"
  in
  Out_channel.(
    with_open_bin "core/prelude.ml" (fun o -> output_string o result))
