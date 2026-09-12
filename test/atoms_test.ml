let arity_errors =
  [
    "(atom)";
    "(atom 1 2)";
    "(deref)";
    "(deref (atom 1) 2)";
    "(reset! (atom 1))";
    "(reset! (atom 1) 2 3)";
    "(swap! (atom 1))";
    "(swap! (atom 1) (fn [x] x) 2)";
  ]

let invalid_references = [ "nil"; "42"; "[1 2]"; "{:value 1}" ]
let invalid_operations = [ "(deref value)"; "(reset! value 9)"; "(swap! value (fn [x] 9))" ]

let eval context input =
  match Frontend.parse_and_desugar input with
  | Ok forms -> Backend_eval.Eval.eval_all ~context forms
  | Error message -> Alcotest.fail message

let check_symbol context input expected =
  match List.rev (eval context input) with
  | Backend_eval.Eval.Symbol value :: _ -> Alcotest.(check string) input expected value
  | _ -> Alcotest.fail "expected symbol"

let expect_eval_error context input =
  match eval context input with
  | exception Backend_eval.Eval.Eval_error _ -> ()
  | _ -> Alcotest.failf "expected eval error: %s" input

let eval_atoms () =
  let context = Backend_eval.Eval.create_context () in
  List.iter (expect_eval_error context) arity_errors;
  List.iter
    (fun value ->
      ignore (eval context ("(def value " ^ value ^ ")"));
      let before = eval context "(str value)" in
      List.iter
        (fun operation ->
          expect_eval_error context operation;
          Alcotest.(check bool) "invalid reference is unchanged" true (before = eval context "(str value)"))
        invalid_operations)
    invalid_references;
  ignore (eval context "(def cell (atom 10)) (def calls (atom 0))");
  check_symbol context "(deref cell)" "10";
  check_symbol context "(reset! cell 20)" "20";
  check_symbol context "(deref cell)" "20";
  check_symbol context "(let [delta 3] (swap! cell (fn [x] (+ x delta))))" "23";
  check_symbol context "(deref cell)" "23";
  List.iter
    (fun expression ->
      expect_eval_error context expression;
      check_symbol context "(deref cell)" "23")
    [ "(swap! cell 42)"; "(swap! cell (fn [x] (reset! calls 1) (deref nil)))" ];
  check_symbol context "(deref calls)" "1";
  check_symbol context "(reset! cell cell) (str cell)" "#<atom>"

let read_file path = In_channel.with_open_text path In_channel.input_all
let write_file path text = Out_channel.with_open_text path (fun output -> output_string output text)

let with_temp_dir f =
  let dir = Filename.temp_dir "language-atoms-" "" in
  let rec remove path =
    if Sys.is_directory path then (
      Array.iter (fun name -> remove (Filename.concat path name)) (Sys.readdir path);
      Unix.rmdir path)
    else Sys.remove path
  in
  Fun.protect ~finally:(fun () -> remove dir) (fun () -> f dir)

let command program args =
  let stdout, stdin, stderr =
    Unix.open_process_args_full program (Array.of_list (program :: args)) (Unix.environment ())
  in
  close_out stdin;
  let output = In_channel.input_all stdout in
  let error = In_channel.input_all stderr in
  (Unix.close_process_full (stdout, stdin, stderr), output, error)

let run program args =
  match command program args with
  | Unix.WEXITED 0, output, _ -> output
  | _, output, error -> Alcotest.failf "%s failed:\n%s\n%s" program output error

let compile target input =
  match Language_main.Runner.run ~target input with Ok source -> source | Error message -> Alcotest.fail message

let operations_source =
  String.concat "\n" (List.mapi (fun i body -> Printf.sprintf "(defn invalid%d [value] %s)" i body) invalid_operations)
  ^ {|
(defn invalid-callback [cell] (swap! cell 42))
(defn failing-callback [cell calls]
  (swap! cell (fn [x] (reset! calls 1) (deref nil))))
(defn update-cell [cell]
  (let [delta 3] (swap! cell (fn [x] (+ x delta)))))
|}

let js_atoms () =
  with_temp_dir (fun dir ->
      write_file (Filename.concat dir "language_runtime.js") (read_file (Sys.getenv "RUNTIME_JS"));
      let arity_source =
        arity_errors |> List.mapi (fun i body -> Printf.sprintf "(defn arity%d [] %s)" i body) |> String.concat "\n"
      in
      let source = compile "js" (operations_source ^ arity_source) in
      let checks =
        {|
import assert from "node:assert/strict";
for (const value of [null, 42, list(1, 2), hash_map("value", 1)]) {
  const before = str(value);
  for (const operation of [invalid0, invalid1, invalid2]) {
    assert.throws(() => operation(value));
    assert.equal(str(value), before);
  }
}
const cell = atom(10), calls = atom(0);
assert.equal(deref(cell), 10);
assert.equal(reset_BANG_(cell, 20), 20);
assert.equal(deref(cell), 20);
assert.equal(update_cell(cell), 23);
assert.equal(deref(cell), 23);
assert.throws(() => invalid_callback(cell));
assert.equal(deref(cell), 23);
assert.throws(() => failing_callback(cell, calls));
assert.equal(deref(cell), 23);
assert.equal(deref(calls), 1);
reset_BANG_(cell, cell);
assert.equal(str(cell), "#<atom>");
|}
        ^ (arity_errors
          |> List.mapi (fun i _ -> Printf.sprintf "assert.throws(() => arity%d());" i)
          |> String.concat "\n")
      in
      let path = Filename.concat dir "checks.mjs" in
      write_file path (source ^ checks);
      ignore (run "node" [ path ]))

let java_atoms () =
  with_temp_dir (fun dir ->
      let source_path = Filename.concat dir "user.java" in
      let runner_path = Filename.concat dir "AtomChecks.java" in
      write_file source_path (compile "java" operations_source);
      write_file runner_path
        {|
import static y2k.language.language_runtime.*;
public final class AtomChecks {
  static void equal(Object actual, Object expected) {
    if (!java.util.Objects.equals(actual, expected))
      throw new AssertionError("expected " + expected + ", got " + actual);
  }
  static void fails(Fn0 action) throws Exception {
    try { action.call(); } catch (Exception expected) { return; }
    throw new AssertionError("expected exception");
  }
  public static void main(String[] args) throws Exception {
    for (Object value : new Object[] {null, 42, list(1, 2), hash_map("value", 1)}) {
      String before = str(value);
      fails(() -> user.invalid0(value));
      equal(str(value), before);
      fails(() -> user.invalid1(value));
      equal(str(value), before);
      fails(() -> user.invalid2(value));
      equal(str(value), before);
    }
    Object cell = atom(10), calls = atom(0);
    equal(deref(cell), 10);
    equal(reset_BANG_(cell, 20), 20);
    equal(deref(cell), 20);
    equal(user.update_cell(cell), 23);
    equal(deref(cell), 23);
    fails(() -> user.invalid_callback(cell));
    equal(deref(cell), 23);
    fails(() -> user.failing_callback(cell, calls));
    equal(deref(cell), 23);
    equal(deref(calls), 1);
    reset_BANG_(cell, cell);
    equal(str(cell), "#<atom>");
  }
}
|};
      ignore (run "javac" [ "-d"; dir; Sys.getenv "RUNTIME_JAVA"; source_path; runner_path ]);
      ignore (run "java" [ "-cp"; dir; "AtomChecks" ]);
      List.iter
        (fun expression ->
          write_file source_path (compile "java" ("(defn test [] " ^ expression ^ ")"));
          match command "javac" [ "-cp"; dir; "-d"; dir; source_path ] with
          | Unix.WEXITED 1, _, error when String.length error > 0 -> ()
          | _, output, error -> Alcotest.failf "expected javac arity error: %s\n%s\n%s" expression output error)
        arity_errors)

let () =
  Alcotest.run "atoms"
    [
      ("eval", [ Alcotest.test_case "operations and errors" `Quick eval_atoms ]);
      ("js", [ Alcotest.test_case "operations and errors" `Slow js_atoms ]);
      ("java", [ Alcotest.test_case "operations and errors" `Slow java_atoms ]);
    ]
