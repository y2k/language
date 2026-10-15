let write_file path text = Out_channel.with_open_bin path (fun output -> output_string output text)

let with_temp_dir f =
  let dir = Filename.temp_dir "language-source-map-" "" in
  let rec remove path =
    if Sys.is_directory path then (
      Array.iter (fun name -> remove (Filename.concat path name)) (Sys.readdir path);
      Unix.rmdir path)
    else Sys.remove path
  in
  Fun.protect ~finally:(fun () -> remove dir) (fun () -> f dir)

let process ?(input = "") program args =
  let stdout, stdin, stderr =
    Unix.open_process_args_full program (Array.of_list (program :: args)) (Unix.environment ())
  in
  output_string stdin input;
  close_out stdin;
  let output = In_channel.input_all stdout in
  let error = In_channel.input_all stderr in
  (Unix.close_process_full (stdout, stdin, stderr), output, error)

let consume ?(source_name = "user.clj") source generated checks =
  with_temp_dir (fun dir ->
      let generated_path = Filename.concat dir "generated.mjs" in
      let source_path = Filename.concat dir "input.clj" in
      let script = Filename.concat dir "check.mjs" in
      write_file generated_path generated;
      write_file source_path source;
      write_file script
        ({|import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { SourceMap } from 'node:module';
const js = readFileSync(process.argv[2], 'utf8');
const match = js.match(/\n\/\/# sourceMappingURL=data:application\/json;base64,([A-Za-z0-9+/=]+)$/);
assert.ok(match, 'final inline map');
const payload = JSON.parse(Buffer.from(match[1], 'base64').toString('utf8'));
assert.equal(payload.version, 3);
assert.deepEqual(payload.sources, [process.argv[4]]);
assert.deepEqual(payload.sourcesContent, [readFileSync(process.argv[3], 'utf8')]);
assert.deepEqual(payload.names, []);
const map = new SourceMap(payload);
function at(line, column, expected) {
  const entry = map.findEntry(line, column);
  assert.deepEqual([entry.originalLine, entry.originalColumn], expected, `at ${line}:${column}`);
}
|}
       ^ checks);
      let status, output, error = process "node" [ script; generated_path; source_path; source_name ] in
      if status <> Unix.WEXITED 0 then Alcotest.failf "Node consumer failed:\n%s%s" output error)

let loc line column = Some Frontend.{ line; column }

let writer_origins () =
  let module Map = Backend_compiler.Source_map in
  let source = "α😀x\r\nsecond\nlast\n\"quoted\" \\ tab\t\000" in
  let writer = Map.create ~source () in
  Map.write writer "prologue\n";
  Map.with_origin writer (loc 3 1) (fun () ->
      Map.write writer "α😀";
      Map.with_origin writer (loc 1 7) (fun () ->
          Map.write writer "x";
          Map.with_origin writer (loc 1 1) (fun () -> Map.write writer "y");
          Map.write writer "z");
      Map.write writer "w");
  Map.write writer "!";
  consume source (Map.finish writer)
    {|at(0, 0, [undefined, undefined]);
at(1, 0, [2, 0]);
at(1, 2, [2, 0]);
at(1, 3, [0, 3]);
at(1, 4, [0, 0]);
at(1, 5, [0, 3]);
at(1, 6, [2, 0]);
at(1, 7, [undefined, undefined]);|}

let writer_lines () =
  let module Map = Backend_compiler.Source_map in
  let source = "first\r\n  origin" in
  let writer = Map.create ~source () in
  Map.with_origin writer (loc 2 3) (fun () ->
      Map.write writer "a\r";
      Map.write writer "\nb\rc\226\128\168d\226\128\169e\nf");
  Map.write writer "\r\ng";
  consume source (Map.finish writer)
    {|for (let line = 0; line < 6; line++) at(line, 0, [1, 2]);
at(6, 0, [undefined, undefined]);|}

let writer_empty_scopes () =
  let module Map = Backend_compiler.Source_map in
  let source = "ab" in
  let writer = Map.create ~source () in
  Map.with_origin writer (loc 1 1) (fun () ->
      Map.with_origin writer (loc 1 2) (fun () -> Map.write writer "");
      Map.write writer "a");
  consume source (Map.finish writer) "at(0, 0, [0, 0]);"

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let cli args source =
  let executable = Filename.concat (Filename.dirname Sys.executable_name) "../bin/main.exe" in
  process ~input:source executable args

let compile_cli ?(source_map = true) ?source_file source =
  let args =
    [ "--target"; "js" ]
    @ (if source_map then [ "--source-map" ] else [])
    @ match source_file with None -> [] | Some path -> [ "--source-file"; path ]
  in
  let status, output, error = cli args source in
  if status <> Unix.WEXITED 0 then Alcotest.failf "CLI failed:\n%s" error;
  (* The CLI adds exactly one newline; preserve every byte emitted by the compiler. *)
  String.sub output 0 (String.length output - 1)

let execute_in dir generated =
  let runtime = Option.value (Sys.getenv_opt "RUNTIME_JS") ~default:"prelude/language_runtime.js" in
  write_file (Filename.concat dir "language_runtime.js") (In_channel.with_open_bin runtime In_channel.input_all);
  let path = Filename.concat dir "program.mjs" in
  write_file path generated;
  process "node" [ "--enable-source-maps"; path ]

let execute generated = with_temp_dir (fun dir -> execute_in dir generated)

let user_frames error =
  error |> String.split_on_char '\n' |> List.map String.trim
  |> List.filter (fun line ->
      String.starts_with ~prefix:"at " line && (contains line ".clj:" || contains line "program.mjs:"))

let check_frame position frame =
  if not (String.ends_with ~suffix:position frame || String.ends_with ~suffix:(position ^ ")") frame) then
    Alcotest.failf "Expected user frame %s, got %s" position frame

let check_failure (status, _, error) kind position =
  Alcotest.(check bool) "nonzero exit" true (match status with Unix.WEXITED n -> n <> 0 | _ -> false);
  if not (contains error ("\n" ^ kind ^ ":")) then Alcotest.failf "Expected %s:\n%s" kind error;
  match user_frames error with
  | [] -> Alcotest.failf "No user frame:\n%s" error
  | first :: _ -> check_frame position first

let failing_function = "(ns demo)\n(defn fail []\n  (.parse JSON \"invalid-json\"))\n(fail)"
let precise_function () = check_failure (execute (compile_cli failing_function)) "SyntaxError" "user.clj:3:3"

let direct_call () =
  check_failure (execute (compile_cli "(ns demo)\n  (.parse JSON \"invalid-json\")")) "SyntaxError" "user.clj:2:3"

let unsupported_target target () =
  let status, output, error = cli [ "--target"; target; "--source-map" ] "42" in
  Alcotest.(check bool) "rejected" true (match status with Unix.WEXITED n -> n <> 0 | _ -> false);
  Alcotest.(check string) "no generated output" "" output;
  Alcotest.(check bool)
    "JS-only diagnostic" true
    (contains error "--source-map" && contains error "only" && contains error "js")

let source_content () =
  let source = "(ns demo)\r\n; Кириллица 😀 \"quotes\" \\ slash\r\n(def text \"Ж😀\")\r\n" in
  let from_runner =
    match Language_main.Runner.run ~source_map:true ~target:"js" source with
    | Ok output -> output
    | Error message -> Alcotest.fail message
  in
  List.iter
    (fun generated -> consume source generated "at(0, 0, [undefined, undefined]);")
    [ compile_cli source; from_runner ]

let compatibility () =
  let source =
    {|(ns demo (:require ["node:path" :as path]))
(def cell (atom 0))
(defn bump [] (swap! cell (fn [x] (+ x 1))))
(println (bump))
(println (bump))
(println (deref cell))
(export-default :value cell)
(raw-code "  // keep final whitespace  ")|}
  in
  let plain = compile_cli ~source_map:false source in
  let mapped = compile_cli source in
  Alcotest.(check bool) "no map by default" false (contains plain "sourceMappingURL");
  Alcotest.(check string) "unchanged executable bytes" plain (String.sub mapped 0 (String.length plain));
  Alcotest.(check bool)
    "only appended map" true
    (String.starts_with ~prefix:"\n//# sourceMappingURL=data:application/json;base64,"
       (String.sub mapped (String.length plain) (String.length mapped - String.length plain)));
  List.iter
    (fun generated ->
      let status, output, error = execute generated in
      Alcotest.(check bool) "success" true (status = Unix.WEXITED 0);
      Alcotest.(check string) "ordered effects" "1\n2\n2\n" output;
      Alcotest.(check string) "no stderr" "" error)
    [ plain; mapped ]

let errors_unchanged () =
  let plain = execute (compile_cli ~source_map:false failing_function) in
  let mapped = execute (compile_cli failing_function) in
  let status, output, error = plain in
  let mapped_status, mapped_output, mapped_error = mapped in
  Alcotest.(check bool) "same exit status" true (status = mapped_status);
  Alcotest.(check string) "same effects" output mapped_output;
  let error_line text = text |> String.split_on_char '\n' |> List.find (String.starts_with ~prefix:"SyntaxError:") in
  Alcotest.(check string) "same error type and message" (error_line error) (error_line mapped_error)

let runtime_frame () =
  let result = execute (compile_cli "(ns demo)\n(defn fail []\n  (re-pattern \"[\"))\n(fail)") in
  check_failure result "Error" "user.clj:3:3";
  let _, _, error = result in
  let frame =
    error |> String.split_on_char '\n' |> List.map String.trim
    |> List.find_opt (fun line -> String.starts_with ~prefix:"at " line && contains line "language_runtime.js:")
  in
  match frame with
  | None -> Alcotest.failf "Missing runtime JS frame:\n%s" error
  | Some frame -> (
      let frame = if String.ends_with ~suffix:")" frame then String.sub frame 0 (String.length frame - 1) else frame in
      match List.rev (String.split_on_char ':' frame) with
      | column :: line :: _ ->
          Alcotest.(check bool)
            "runtime retains JS line and column" true
            (int_of_string line > 0 && int_of_string column > 0)
      | _ -> Alcotest.fail frame)

let prologue () =
  let source =
    "(ns demo (:require [\"node:path\" :as path]) (:require [\"node:url\" :as url]))\n"
    ^ "(defn fail []\n  (.parse JSON \"invalid\"))\n(fail)"
  in
  check_failure (execute (compile_cli source)) "SyntaxError" "user.clj:3:3"

let unicode_crlf () =
  let source = "(ns demo)\r\n(println \"Ж😀\")   (.parse JSON \"invalid\")" in
  check_failure (execute (compile_cli source)) "SyntaxError" "user.clj:2:19"

let named_source source_file source_name relative_path () =
  with_temp_dir (fun dir ->
      let dir = Unix.realpath dir in
      let dist = Filename.concat dir "dist" in
      Unix.mkdir dist 0o755;
      Unix.mkdir (Filename.concat dir "src") 0o755;
      let source = "(ns demo)\n  (.parse JSON \"invalid-json\")" in
      let generated = compile_cli ?source_file source in
      consume ~source_name source generated "";
      check_failure (execute_in dist generated) "SyntaxError" (Filename.concat dir relative_path ^ ":2:3"))

let source_file_is_not_read () =
  with_temp_dir (fun dir ->
      let source_file = Filename.concat (Unix.realpath dir) "input.clj" in
      let source = "(ns demo)\r\n  (.parse JSON \"invalid-json\")\r\n" in
      let missing = compile_cli ~source_file source in
      let different_content = "This file is not the input. (" in
      write_file source_file different_content;
      let existing = compile_cli ~source_file source in
      Alcotest.(check string) "existing file cannot change the map or program" missing existing;
      let runner =
        match Language_main.Runner.run ~target:"js" ~source_map:true ~source_file source with
        | Ok output -> output
        | Error message -> Alcotest.fail message
      in
      List.iter
        (fun generated ->
          consume ~source_name:source_file source generated "";
          check_failure (execute generated) "SyntaxError" (source_file ^ ":2:3"))
        [ existing; runner ];
      Alcotest.(check string)
        "source file untouched" different_content
        (In_channel.with_open_bin source_file In_channel.input_all))

let source_file_without_map () =
  List.iter
    (fun target ->
      let source = "(defn result [] (let [x 1] (+ x 2)))" in
      let status, output, error = cli [ "--target"; target ] source in
      let named_status, named_output, named_error =
        cli [ "--target"; target; "--source-file"; "missing/a#b?%20.clj" ] source
      in
      Alcotest.(check bool) (target ^ " succeeds") true (status = Unix.WEXITED 0 && named_status = status);
      Alcotest.(check string) (target ^ " unchanged output") output named_output;
      Alcotest.(check string) (target ^ " unchanged diagnostics") error named_error)
    [ "js"; "eval"; "java" ]

let call_cases =
  [
    ("direct call", "(JSON/parse \"invalid\")", 3);
    ("member call", "(.parse JSON \"invalid\")", 3);
    ("constructor call", "(RegExp. \"[\")", 3);
    ("inner argument throws", "(.parse JSON (.parse JSON \"invalid\"))", 16);
    ("outer call after successful argument", "(.parse JSON (str \"invalid\"))", 3);
    ("outer call after successful receiver", "(.parse ((fn [] JSON)) \"invalid\")", 3);
    ("receiver throws", "(.parse (.parse JSON \"invalid\") \"unused\")", 11);
    ("outer call after successful callee", "(((fn [] JSON/parse)) \"invalid\")", 3);
    ("callee throws", "((.parse JSON \"invalid\"))", 4);
    ("two calls on one line", "(str \"ok\") (.parse JSON \"invalid\")", 14);
  ]

let call_position expression column () =
  let source = "(ns demo)\n(defn fail []\n  " ^ expression ^ ")\n(fail)" in
  check_failure (execute (compile_cli source)) "SyntaxError" (Printf.sprintf "user.clj:3:%d" column)

let call_chain () =
  let source = "(ns demo)\n(defn fail []\n  (.parse JSON \"invalid\"))\n(defn middle []\n  (fail))\n(middle)" in
  let result = execute (compile_cli source) in
  check_failure result "SyntaxError" "user.clj:3:3";
  let _, _, error = result in
  match user_frames error with
  | first :: second :: third :: _ ->
      List.iter2 check_frame [ "user.clj:3:3"; "user.clj:5:3"; "user.clj:6:1" ] [ first; second; third ]
  | _ -> Alcotest.failf "Missing caller frames:\n%s" error

let transformed_cases =
  [
    ("thread first", {|(ns demo)
(defn fail []
  (-> JSON
      (.parse "invalid")))
(fail)|}, 4, 7);
    ("thread last", {|(ns demo)
(defn fail []
  (->> "invalid"
       (.parse JSON)))
(fail)|}, 4, 8);
    ( "let destructuring with if/do",
      {|(ns demo)
(defn fail []
  (let [[value] ["invalid"]]
    (if true
      (do
        (str "ok")
        (.parse JSON value))
      nil)))
(fail)|},
      7,
      9 );
    ( "let initializer",
      {|(ns demo)
(defn fail []
  (let [value
        (.parse JSON "invalid")]
    value))
(fail)|},
      4,
      9 );
    ( "nested parameter destructuring",
      {|(ns demo)
(defn fail [{:data [value]}]
  (.parse JSON value))
(fail {:data ["invalid"]})|},
      3,
      3 );
    ("callback", {|(ns demo)
(run! (fn [value]
        (.parse JSON value))
      ["invalid"])|}, 3, 9);
    ( "Promise callback",
      {|(ns demo)
(.then (Promise/resolve "invalid")
       (fn [value]
         (.parse JSON value)))|},
      4,
      10 );
    ( "Unicode and CRLF inside function",
      "(ns demo)\r\n(defn fail []\r\n  (str \"Ж😀\") (.parse JSON \"invalid\"))\r\n(fail)",
      3,
      15 );
  ]

let transformed_position source line column () =
  check_failure (execute (compile_cli source)) "SyntaxError" (Printf.sprintf "user.clj:%d:%d" line column)

let raw_code_position separator inside () =
  let payload = "void 0;" ^ separator ^ if inside then "JSON.parse('invalid');" else "void 0;" in
  let source =
    "(ns demo)\n(defn fail []\n  (raw-code "
    ^ Backend_compiler.Target_syntax.string_literal payload
    ^ ")\n"
    ^ (if inside then "  \"done\")" else "  (.parse JSON \"invalid\"))")
    ^ "\n(fail)"
  in
  check_failure (execute (compile_cli source)) "SyntaxError" (if inside then "user.clj:3:3" else "user.clj:4:3")

let named_expression () =
  with_temp_dir (fun dir ->
      let dir = Unix.realpath dir in
      let dist = Filename.concat dir "dist" in
      Unix.mkdir dist 0o755;
      let generated = compile_cli ~source_file:"../src/my #?%20.clj" failing_function in
      consume ~source_name:"../src/my %23%3F%2520.clj" failing_function generated "";
      check_failure (execute_in dist generated) "SyntaxError" (Filename.concat dir "src/my #?%20.clj:3:3"))

let () =
  Alcotest.run "JS source maps"
    [
      ( "writer",
        [
          Alcotest.test_case "scoped origins, deltas, UTF-16 and source content" `Quick writer_origins;
          Alcotest.test_case "JS line terminators and split CRLF" `Quick writer_lines;
          Alcotest.test_case "empty scopes at the same position" `Quick writer_empty_scopes;
        ] );
      ( "stage 1",
        [
          Alcotest.test_case "exact CLI and runner input content" `Quick source_content;
          Alcotest.test_case "unchanged JS and ordered effects" `Quick compatibility;
          Alcotest.test_case "unchanged errors" `Quick errors_unchanged;
          Alcotest.test_case "runtime retains its JS frame" `Quick runtime_frame;
          Alcotest.test_case "runtime and namespace import prologue" `Quick prologue;
          Alcotest.test_case "top-level UTF-16 position after Unicode and CRLF" `Quick unicode_crlf;
        ] );
      ( "CLI",
        [
          Alcotest.test_case "function maps to failing expression" `Quick precise_function;
          Alcotest.test_case "direct call maps to its start" `Quick direct_call;
          Alcotest.test_case "reject eval source map" `Quick (unsupported_target "eval");
          Alcotest.test_case "reject Java source map" `Quick (unsupported_target "java");
        ] );
      ( "expression calls",
        List.map
          (fun (name, expression, column) -> Alcotest.test_case name `Quick (call_position expression column))
          call_cases
        @ [ Alcotest.test_case "chain of user frames" `Quick call_chain ] );
      ( "source file",
        [
          Alcotest.test_case "default beside generated JS" `Quick (named_source None "user.clj" "dist/user.clj");
          Alcotest.test_case "relative path with spaces outside dist" `Quick
            (named_source (Some "../src/my demo.clj") "../src/my demo.clj" "src/my demo.clj");
          Alcotest.test_case "literal hash" `Quick (named_source (Some "a#b.clj") "a%23b.clj" "dist/a#b.clj");
          Alcotest.test_case "literal question mark" `Quick (named_source (Some "a?b.clj") "a%3Fb.clj" "dist/a?b.clj");
          Alcotest.test_case "literal percent escape" `Quick
            (named_source (Some "a%20b.clj") "a%2520b.clj" "dist/a%20b.clj");
          Alcotest.test_case "absolute path is metadata, stdin is input" `Quick source_file_is_not_read;
          Alcotest.test_case "name alone has no effect on any target" `Quick source_file_without_map;
          Alcotest.test_case "combined named path and expression precision" `Quick named_expression;
        ] );
      ( "transformed expressions",
        List.map
          (fun (name, source, line, column) -> Alcotest.test_case name `Quick (transformed_position source line column))
          transformed_cases );
      ( "raw code",
        List.concat_map
          (fun (name, separator) ->
            [
              Alcotest.test_case (name ^ " inside payload") `Quick (raw_code_position separator true);
              Alcotest.test_case (name ^ " following payload") `Quick (raw_code_position separator false);
            ])
          [ ("LF", "\n"); ("CR", "\r"); ("CRLF", "\r\n"); ("U+2028", "\226\128\168"); ("U+2029", "\226\128\169") ] );
    ]
