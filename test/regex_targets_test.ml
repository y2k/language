let read_file path = In_channel.with_open_text path In_channel.input_all
let write_file path text = Out_channel.with_open_text path (fun output -> output_string output text)

let with_temp_dir f =
  let dir = Filename.temp_dir "language-regex-targets-" "" in
  let rec remove path =
    if Sys.is_directory path then (
      Array.iter (fun name -> remove (Filename.concat path name)) (Sys.readdir path);
      Unix.rmdir path)
    else Sys.remove path
  in
  Fun.protect ~finally:(fun () -> remove dir) (fun () -> f dir)

let process program args =
  let stdout, stdin, stderr =
    Unix.open_process_args_full program (Array.of_list (program :: args)) (Unix.environment ())
  in
  close_out stdin;
  let output = In_channel.input_all stdout in
  let error = In_channel.input_all stderr in
  (Unix.close_process_full (stdout, stdin, stderr), output, error)

let run program args =
  let status, output, error = process program args in
  if status <> Unix.WEXITED 0 then Alcotest.failf "%s failed:\n%s\n%s" program output error;
  output

let compile target source =
  match Language_main.Runner.run ~target source with Ok output -> output | Error message -> Alcotest.fail message

let namespace_source =
  {|(ns checks.regex.core)
(defn test []
  (let [r (re-pattern "foo[0-9]+")]
    (str (re-find r "xfoo42") "|" (re-replace "foo1 foo2" r "X"))))|}

let namespace target () =
  with_temp_dir (fun dir ->
      let output =
        if target = "js" then (
          write_file (Filename.concat dir "language_runtime.js") (read_file (Sys.getenv "RUNTIME_JS"));
          let checks = Filename.concat dir "checks" in
          Unix.mkdir checks 0o755;
          let regex = Filename.concat checks "regex" in
          Unix.mkdir regex 0o755;
          let path = Filename.concat regex "core.mjs" in
          write_file path (compile target (namespace_source ^ "\n(println (test))"));
          run "node" [ path ])
        else
          let path = Filename.concat dir "core.java" in
          let runner = Filename.concat dir "RegexChecks.java" in
          write_file path (compile target namespace_source);
          write_file runner
            {|public class RegexChecks {
public static void main(String[] args) throws Exception {
  System.out.println(checks.regex.core.test());
}
}|};
          ignore (run "javac" [ "-d"; dir; Sys.getenv "RUNTIME_JAVA"; path; runner ]);
          run "java" [ "-cp"; dir; "RegexChecks" ]
      in
      Alcotest.(check string) "nested namespace API" "foo42|X X\n" output)

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let errors =
  [
    ("re-pattern", "expects", "(re-pattern)");
    ("re-pattern", "expects", {|(re-pattern "x" "flags")|});
    ("re-find", "expects", "(re-find)");
    ("re-find", "expects", {|(re-find (re-pattern "x"))|});
    ("re-find", "expects", {|(re-find (re-pattern "x") "x" "extra")|});
    ("re-replace", "expects", "(re-replace)");
    ("re-replace", "expects", {|(re-replace "" (re-pattern "x"))|});
    ("re-replace", "expects", {|(re-replace "" (re-pattern "x") "-" "extra")|});
    ("re-pattern", "invalid", {|(re-pattern "[")|});
  ]
  @ List.concat_map
      (fun value ->
        [
          ("re-pattern", "expects", "(re-pattern " ^ value ^ ")");
          ("re-find", "expects", "(re-find (re-pattern \"x\") " ^ value ^ ")");
          ("re-replace", "expects", "(re-replace " ^ value ^ " (re-pattern \"x\") \"-\")");
        ]
        @ List.concat_map
            (fun text ->
              [
                ("re-find", "expects", Printf.sprintf "(re-find %s %s)" value text);
                ("re-replace", "expects", Printf.sprintf "(re-replace %s %s \"-\")" text value);
                ("re-replace", "expects", Printf.sprintf "(re-replace %s (re-pattern \"x\") %s)" text value);
              ])
            [ {|""|}; {|"absent"|} ])
      [ "nil"; "42"; "false"; "[]"; "{}"; "(fn [x] x)" ]
  @ [
      ("re-find", "expects", {|(re-find "x" "")|});
      ("re-replace", "expects", {|(re-replace "" "x" "-")|});
      ("", "", {|(let [r (re-pattern "x")] (r))|});
    ]

let error_source =
  errors
  |> List.mapi (fun i (_, _, expression) -> Printf.sprintf "(defn invalid%d [] %s)" i expression)
  |> String.concat "\n"

let check_unhandled program args index operation reason =
  let status, output, error = process program (args @ [ string_of_int index ]) in
  Alcotest.(check bool) "nonzero exit" true (match status with Unix.WEXITED n -> n <> 0 | _ -> false);
  Alcotest.(check string) "no success output" "" output;
  Alcotest.(check bool) "stderr builtin and reason" true (contains error operation && contains error reason)

let contracts target () =
  with_temp_dir (fun dir ->
      let program, args =
        if target = "js" then (
          write_file (Filename.concat dir "language_runtime.js") (read_file (Sys.getenv "RUNTIME_JS"));
          let path = Filename.concat dir "checks.mjs" in
          let cases =
            errors
            |> List.mapi (fun i (operation, reason, _) -> Printf.sprintf "[invalid%d, %S, %S]" i operation reason)
            |> String.concat ",\n"
          in
          write_file path
            (compile target error_source ^ "\nimport assert from 'node:assert/strict';\nconst cases = [" ^ cases
           ^ {|];
if (process.argv.length > 2) cases[Number(process.argv[2])][0]();
for (const [fn, operation, reason] of cases) {
  assert.throws(fn, error => error instanceof Error &&
    error.message.includes(operation) && error.message.includes(reason), fn.name);
}
|}
            );
          ("node", [ path ]))
        else
          let path = Filename.concat dir "user.java" in
          let runner = Filename.concat dir "RegexChecks.java" in
          write_file path (compile target error_source);
          let functions =
            errors |> List.mapi (fun i _ -> Printf.sprintf "() -> user.invalid%d()" i) |> String.concat ",\n"
          in
          let checks =
            errors
            |> List.mapi (fun i (operation, reason, _) ->
                Printf.sprintf "fails(cases[%d], %S, %S, %d);" i operation reason i)
            |> String.concat "\n"
          in
          write_file runner
            ({|import static y2k.language.language_runtime.*;
public class RegexChecks {
  static void fails(Fn0 fn, String operation, String reason, int index) throws Exception {
    try { fn.call(); }
    catch (RuntimeException error) {
      String message = error.getMessage();
      if (message != null && message.contains(operation) && message.contains(reason)) return;
      throw new AssertionError("wrong error in case " + index, error);
    }
    throw new AssertionError("missing error in case " + index);
  }
  public static void main(String[] args) throws Exception {
    Fn0[] cases = new Fn0[] {
|}
           ^ functions ^ "\n};\nif (args.length > 0) cases[Integer.parseInt(args[0])].call();\n" ^ checks ^ "\n}\n}\n");
          ignore (run "javac" [ "-d"; dir; Sys.getenv "RUNTIME_JAVA"; path; runner ]);
          ("java", [ "-cp"; dir; "RegexChecks" ])
      in
      Alcotest.(check string) "all contracts rejected" "" (run program args);
      List.iter
        (fun index ->
          let operation, reason, _ = List.nth errors index in
          check_unhandled program args index operation reason)
        [ 0; 8; 9 ])

let () =
  Alcotest.run "regex targets"
    [
      ("namespace", List.map (fun target -> Alcotest.test_case target `Slow (namespace target)) [ "js"; "java" ]);
      ("contracts", List.map (fun target -> Alcotest.test_case target `Slow (contracts target)) [ "js"; "java" ]);
    ]
