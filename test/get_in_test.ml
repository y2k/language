let invalid_expressions =
  List.map (fun value -> "(get-in {:chat " ^ value ^ "} [:chat :id])") [ "42"; "\"hello\""; "false"; "true" ]
  @ List.concat_map
      (fun path -> List.map (fun collection -> "(get-in " ^ collection ^ " " ^ path ^ ")") [ "{}"; "nil" ])
      [ "nil"; "{}"; "\"path\""; "42"; "false"; "true" ]

let source =
  invalid_expressions
  |> List.mapi (fun i expression -> Printf.sprintf "(defn invalid%d [] %s)" i expression)
  |> String.concat "\n"

let eval_errors () =
  List.iter
    (fun expression ->
      let forms =
        match Frontend.parse_and_desugar expression with Ok forms -> forms | Error message -> Alcotest.fail message
      in
      match Backend_eval.Eval.eval_all forms with
      | exception Backend_eval.Eval.Eval_error _ -> ()
      | _ -> Alcotest.failf "expected runtime error: %s" expression)
    invalid_expressions

let read_file path = In_channel.with_open_text path In_channel.input_all
let write_file path text = Out_channel.with_open_text path (fun output -> output_string output text)

let with_temp_dir f =
  let dir = Filename.temp_dir "language-get-in-" "" in
  let rec remove path =
    if Sys.is_directory path then (
      Array.iter (fun name -> remove (Filename.concat path name)) (Sys.readdir path);
      Unix.rmdir path)
    else Sys.remove path
  in
  Fun.protect ~finally:(fun () -> remove dir) (fun () -> f dir)

let run program args =
  let stdout, stdin, stderr =
    Unix.open_process_args_full program (Array.of_list (program :: args)) (Unix.environment ())
  in
  close_out stdin;
  let output = In_channel.input_all stdout in
  let error = In_channel.input_all stderr in
  match Unix.close_process_full (stdout, stdin, stderr) with
  | Unix.WEXITED 0 -> ()
  | _ -> Alcotest.failf "%s failed:\n%s\n%s" program output error

let compile target =
  match Language_main.Runner.run ~target source with Ok source -> source | Error message -> Alcotest.fail message

let js_checks () =
  with_temp_dir (fun dir ->
      write_file (Filename.concat dir "language_runtime.js") (read_file (Sys.getenv "RUNTIME_JS"));
      let checks =
        invalid_expressions
        |> List.mapi (fun i expression -> Printf.sprintf "assert.throws(() => invalid%d(), Error, %S);" i expression)
        |> String.concat "\n"
      in
      let path = Filename.concat dir "checks.mjs" in
      write_file path
        (compile "js" ^ "\nimport assert from 'node:assert/strict';\n" ^ checks
       ^ {|
const object = {missingValue: undefined, disabled: false, zero: 0, empty: ""};
assert.equal(get(object, "absent"), null);
assert.equal(get(object, "missingValue"), null);
assert.equal(get(object, "disabled"), false);
assert.equal(get(object, "zero"), 0);
assert.equal(get(object, "empty"), "");
assert.equal(get_in(object, ["absent", "id"]), null);
assert.equal(get_in(object, ["missingValue", "id"]), null);
assert.equal(get_in(object, []), object);
|}
        );
      run "node" [ path ])

let java_errors () =
  with_temp_dir (fun dir ->
      let source_path = Filename.concat dir "user.java" in
      let checks_path = Filename.concat dir "GetInChecks.java" in
      write_file source_path (compile "java");
      let checks =
        invalid_expressions
        |> List.mapi (fun i _ -> Printf.sprintf "fails(() -> user.invalid%d());" i)
        |> String.concat "\n"
      in
      write_file checks_path
        ({|
import static y2k.language.language_runtime.*;
public final class GetInChecks {
  static void fails(Fn0 action) throws Exception {
    try { action.call(); } catch (Exception expected) { return; }
    throw new AssertionError("expected runtime exception");
  }
  public static void main(String[] args) throws Exception {
|}
       ^ checks ^ "\n}\n}\n");
      run "javac" [ "-d"; dir; Sys.getenv "RUNTIME_JAVA"; source_path; checks_path ];
      run "java" [ "-cp"; dir; "GetInChecks" ])

let () =
  Alcotest.run "get-in"
    [
      ("eval", [ Alcotest.test_case "invalid arguments" `Quick eval_errors ]);
      ("js", [ Alcotest.test_case "invalid arguments and object lookup" `Slow js_checks ]);
      ("java", [ Alcotest.test_case "invalid arguments" `Slow java_errors ]);
    ]
