let read_file path = In_channel.with_open_text path In_channel.input_all
let write_file path content = Out_channel.with_open_text path (fun output -> output_string output content)

let expected_output path =
  In_channel.with_open_text path (fun input ->
      match input_line input with
      | line when String.starts_with ~prefix:";;" line -> String.trim (String.sub line 2 (String.length line - 2))
      | line -> Alcotest.failf "%s: expected first line to start with ;;, got %S" path line
      | exception End_of_file -> Alcotest.failf "%s: empty sample" path)

let sample_body path =
  match String.split_on_char '\n' (read_file path) with
  | _ :: lines -> String.trim (String.concat "\n" lines)
  | [] -> Alcotest.failf "%s: empty sample" path

let eval_input path = sample_body path ^ "\n(test)\n"
let compiler_input path = sample_body path ^ "\n(print-result (test))\n"
let java_input path = sample_body path
let trim_output = String.trim

let run_command_full ?failure_context main args input =
  let stdout, stdin, stderr = Unix.open_process_args_full main (Array.of_list (main :: args)) (Unix.environment ()) in
  output_string stdin input;
  close_out stdin;
  let output = In_channel.input_all stdout in
  let error = In_channel.input_all stderr in
  let error = match failure_context with Some context -> error ^ "\n" ^ context | None -> error in
  match Unix.close_process_full (stdout, stdin, stderr) with
  | Unix.WEXITED 0 -> (output, error)
  | Unix.WEXITED code -> Alcotest.failf "%s exited with code %d\nstderr: %s" main code error
  | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
      Alcotest.failf "%s stopped by signal %d\nstderr: %s" main signal error

let run_command main args input = fst (run_command_full main args input)

let run_language target input =
  match Language_main.Runner.run ~target input with
  | Ok output -> output
  | Error message -> Alcotest.failf "compiler failed: %s" message

let run path = trim_output (run_language "eval" (eval_input path))
let ensure_runtime_js () = write_file "language_runtime.js" (read_file (Sys.getenv "RUNTIME_JS"))

let run_js path =
  ensure_runtime_js ();
  compiler_input path |> run_language "js" |> run_command "node" [ "--input-type=module" ] |> trim_output

let java_class_name source =
  let prefix = "public final class " in
  match
    source |> String.split_on_char '\n'
    |> List.find_map (fun line ->
        let line = String.trim line in
        if String.starts_with ~prefix line then
          let rest = String.sub line (String.length prefix) (String.length line - String.length prefix) in
          Some (List.hd (String.split_on_char ' ' rest))
        else None)
  with
  | Some name -> name
  | None -> Alcotest.fail "generated Java class not found"

let java_package source =
  let prefix = "package " in
  source |> String.split_on_char '\n'
  |> List.find_map (fun line ->
      let line = String.trim line in
      if String.starts_with ~prefix line then
        let rest = String.sub line (String.length prefix) (String.length line - String.length prefix) in
        Some (String.sub rest 0 (String.length rest - 1))
      else None)
  |> Option.value ~default:""

let run_java path =
  let tmp = Filename.temp_dir "language-java-" "" in
  let out = Filename.concat tmp "out" in
  Unix.mkdir out 0o755;
  let main_source = run_language "java" (java_input path) in
  let package = java_package main_source in
  let class_name = java_class_name main_source in
  let main_java = Filename.concat tmp (class_name ^ ".java") in
  let runner_java = Filename.concat tmp "TestRunner.java" in
  let runner_name = if package = "" then "TestRunner" else package ^ ".TestRunner" in
  let failure_context = String.concat "\n" [ "[generated java]\n"; main_source ] in
  write_file main_java main_source;
  write_file runner_java
    (String.concat "\n"
       ((if package = "" then [] else [ "package " ^ package ^ ";" ])
       @ [
           "import y2k.language.language_runtime;";
           "final class TestRunner {";
           "public static void main(String[] args) throws Exception {";
           "language_runtime.print_result(" ^ class_name ^ ".test());";
           "}";
           "}";
         ]));
  run_command_full ~failure_context "javac" [ "-d"; out; Sys.getenv "RUNTIME_JAVA"; main_java; runner_java ] ""
  |> ignore;
  run_command_full ~failure_context "java" [ "-cp"; out; runner_name ] "" |> fst |> trim_output

let clj_files dir =
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun name -> Filename.check_suffix name ".clj")
  |> List.sort String.compare
  |> List.map (Filename.concat dir)

let clj_files_if_exists dir = if Sys.file_exists dir && Sys.is_directory dir then clj_files dir else []
let target_files samples target = clj_files_if_exists (Filename.concat samples target)

let relative_path ~root path =
  let prefix = if String.ends_with ~suffix:Filename.dir_sep root then root else root ^ Filename.dir_sep in
  if String.starts_with ~prefix path then
    String.sub path (String.length prefix) (String.length path - String.length prefix)
  else Filename.basename path

let sample_test samples path =
  Alcotest.test_case (relative_path ~root:samples path) `Slow (fun () ->
      Alcotest.(check string) path (expected_output path) (run path))

let js_sample_test samples path =
  Alcotest.test_case (relative_path ~root:samples path) `Slow (fun () ->
      Alcotest.(check string) path (expected_output path) (run_js path))

let java_sample_test samples path =
  Alcotest.test_case (relative_path ~root:samples path) `Slow (fun () ->
      Alcotest.(check string) path (expected_output path) (run_java path))

let () =
  let samples = Sys.getenv "SAMPLES_DIR" in
  match clj_files samples with
  | [] -> Alcotest.failf "no .clj files found in %s" samples
  | common_files ->
      let eval_files = common_files @ target_files samples "eval" in
      let js_files = common_files @ target_files samples "js" in
      let java_files = common_files @ target_files samples "java" in
      Alcotest.run "samples"
        [
          ("eval", List.map (sample_test samples) eval_files);
          ("js", List.map (js_sample_test samples) js_files);
          ("java", List.map (java_sample_test samples) java_files);
        ]
