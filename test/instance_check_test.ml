let parse source =
  match Frontend.parse_and_desugar source with Ok forms -> forms | Error message -> Alcotest.fail message

let unsupported_targets () =
  let message = "instance? is supported only on the Java target" in
  Alcotest.check_raises "JS diagnostic" (Failure message) (fun () ->
      ignore (Backend_compiler.Js.compile (parse "(defn test [value] (instance? String value))")));
  let context = Backend_eval.Eval.create_context () in
  let eval source = Backend_eval.Eval.eval_all ~context (parse source) in
  ignore (eval "(def counter (atom 0))");
  Alcotest.check_raises "eval diagnostic" (Backend_eval.Eval.Eval_error message) (fun () ->
      ignore (eval "(instance? String (reset! counter 1))"));
  Alcotest.(check bool) "argument not evaluated" true (eval "(deref counter)" = [ Backend_eval.Eval.Symbol "0" ])

let write_file path text = Out_channel.with_open_text path (fun output -> output_string output text)

let with_temp_dir f =
  let dir = Filename.temp_dir "language-instance-check-" "" in
  let rec remove path =
    if Sys.is_directory path then (
      Array.iter (fun name -> remove (Filename.concat path name)) (Sys.readdir path);
      Unix.rmdir path)
    else Sys.remove path
  in
  Fun.protect ~finally:(fun () -> remove dir) (fun () -> f dir)

let run ?(success = true) program args =
  let stdout, stdin, stderr =
    Unix.open_process_args_full program (Array.of_list (program :: args)) (Unix.environment ())
  in
  close_out stdin;
  let output = In_channel.input_all stdout in
  let error = In_channel.input_all stderr in
  let status = Unix.close_process_full (stdout, stdin, stderr) in
  match (success, status) with
  | true, Unix.WEXITED 0 | false, Unix.WEXITED 1 -> ()
  | _ -> Alcotest.failf "%s unexpected status:\n%s\n%s" program output error

let java_checks () =
  with_temp_dir (fun dir ->
      let source_path = Filename.concat dir "user.java" in
      let runner_path = Filename.concat dir "InstanceChecks.java" in
      let source =
        {|(ns user (:import [java.util ArrayList]))
(def counter (atom 0))
(defn make-value [] (swap! counter (fn [n] (+ n 1))) "value")
(defn imported [] (instance? ArrayList (ArrayList.)))
(defn subclass [] (instance? java.util.AbstractList (ArrayList.)))
(defn interface-check [] (instance? java.util.List (ArrayList.)))
(defn nil-check [] (instance? String nil))
(defn unrelated [] (instance? Integer "hello"))
(defn integer-check [] (instance? Integer 42))
(defn boolean-check [] (instance? Boolean true))
(defn scalar-unrelated [] (instance? String 42))
(defn single [] (instance? String (make-value)))
(defn control [] (instance? String (let [x (make-value)] (do (if true x nil)))))
(defn condition [] (if (instance? String (make-value)) "yes" "no"))
(defn argument [] (= true (instance? String (make-value))))
(defn binding [] (let [result (instance? String (make-value))] result))
(defn discarded [] (instance? Object (make-value)) "done")
(defn skipped [] (if false (instance? Object (make-value)) "skipped"))
(defn runnable [] ^void:java.lang.Runnable (fn [] (instance? Object (make-value))))
(defn discarded-block [] (do (instance? Object (make-value)) "block"))
(defn read-count [] (deref counter))
|}
      in
      write_file source_path (Backend_compiler.Java.compile (parse source));
      write_file runner_path
        {|public final class InstanceChecks {
  static void check(Object expected, Object actual) {
    if (!expected.equals(actual)) throw new AssertionError("expected " + expected + ", got " + actual);
  }
  public static void main(String[] args) throws Exception {
    check(true, user.imported());
    check(true, user.subclass());
    check(true, user.interface_check());
    check(false, user.nil_check());
    check(false, user.unrelated());
    check(true, user.integer_check());
    check(true, user.boolean_check());
    check(false, user.scalar_unrelated());
    check(true, user.single()); check(1, user.read_count());
    check(true, user.control()); check(2, user.read_count());
    check("yes", user.condition()); check(3, user.read_count());
    check(true, user.argument()); check(4, user.read_count());
    check(true, user.binding()); check(5, user.read_count());
    check("done", user.discarded()); check(6, user.read_count());
    check("skipped", user.skipped()); check(6, user.read_count());
    ((Runnable) user.runnable()).run(); check(7, user.read_count());
    check("block", user.discarded_block()); check(8, user.read_count());
  }
}
|};
      run "javac" [ "-d"; dir; Sys.getenv "RUNTIME_JAVA"; source_path; runner_path ];
      run "java" [ "-cp"; dir; "InstanceChecks" ];
      write_file source_path
        (Backend_compiler.Java.compile (parse "(defn missing [value] (instance? missing.types.NoSuchType value))"));
      run ~success:false "javac" [ "-cp"; dir; "-d"; dir; source_path ])

let () =
  Alcotest.run "instance?"
    [
      ("targets", [ Alcotest.test_case "unsupported targets" `Quick unsupported_targets ]);
      ("java", [ Alcotest.test_case "types and single evaluation" `Slow java_checks ]);
    ]
