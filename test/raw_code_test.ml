let parse source =
  match Frontend.parse_and_desugar source with Ok forms -> forms | Error message -> Alcotest.fail message

let contains text part =
  let length = String.length part in
  List.exists (fun i -> String.sub text i length = part) (List.init (max 0 (String.length text - length + 1)) Fun.id)

let check_contains label text part = Alcotest.(check bool) label true (contains text part)
let js_compile source = Backend_compiler.Js.compile (parse source)
let java_compile source = Backend_compiler.Java.compile (parse source)

let js_generation () =
  let module_code = js_compile {|(raw-code "export const hostValue = 42;")|} in
  Alcotest.(check bool)
    "top-level payload without terminator" true
    (String.ends_with ~suffix:"\nexport const hostValue = 42;" module_code);
  let code = js_compile {|(defn test [] (raw-code "console.log('hello');") "done")|} in
  check_contains "statement before return" code "\nconsole.log('hello');\nreturn \"done\";";
  Alcotest.(check bool) "no runtime raw-code call" false (contains code "raw_code");
  let empty = js_compile {|(defn test [] (raw-code "") "done")|} in
  check_contains "empty statement adds no semicolon" empty "{\n\nreturn \"done\";";
  let quoted = js_compile {|(def data '(raw-code "host();"))|} in
  check_contains "quote stays data" quoted {|list("raw-code", "host();")|}

let decoded_payload = "  // punctuation !?+- /\n\t\r\"\\\\n\\q\n// tail"

let js_discarded_branches () =
  let code = js_compile {|(defn test [] (let [] (if false (do (println "x") nil) nil) "done"))|} in
  Alcotest.(check bool) "lowered binding is not a function call" false (contains code "let_STAR_");
  Alcotest.(check bool)
    "statement block does not introduce a function scope" false (contains code "(() => {\n(println)")

let js_escapes () =
  let code = js_compile {|(raw-code "  // punctuation !?+- /
\t\r\"\\\\n\q\n// tail") (def after 42)|} in
  check_contains "exact decoded payload and line-comment boundary" code
    ("\n" ^ decoded_payload ^ "\nexport const after = 42;")

let invalid_forms =
  [
    ("(raw-code)", "one string literal");
    ({|(raw-code "a" "b")|}, "one string literal");
    ("(raw-code :text)", "string literal");
    ("(raw-code text)", "string literal");
    ("(raw-code 42)", "string literal");
    ("(raw-code nil)", "string literal");
    ("(raw-code true)", "string literal");
    ({|(raw-code (str "text"))|}, "string literal");
  ]

let diagnostics compile () =
  List.iter
    (fun (source, reason) ->
      match compile ("\n  " ^ source) with
      | exception Failure message ->
          check_contains "names raw-code" message "raw-code:";
          check_contains "reason" message reason;
          check_contains "line" message "line 2";
          check_contains "column" message "column "
      | _ -> Alcotest.failf "accepted invalid form: %s" source)
    invalid_forms

let java_generation () =
  let module_code = java_compile {|(raw-code "public static int hostValue = 42;")|} in
  check_contains "class member at top level" module_code
    "public final class user {\npublic static int hostValue = 42;\n}";
  Alcotest.(check bool) "no automatic initializer" false (contains module_code "static {");
  let code = java_compile {|(defn test [] (raw-code "System.out.println(\"hello\");") "done")|} in
  check_contains "statement before return" code "\nSystem.out.println(\"hello\");\nreturn \"done\";";
  Alcotest.(check bool) "no runtime raw-code call" false (contains code "raw_code");
  let empty = java_compile {|(defn test [] (raw-code "") "done")|} in
  check_contains "empty statement adds no semicolon" empty "{\n\nreturn \"done\";";
  let quoted = java_compile {|(def data '(raw-code "host();"))|} in
  check_contains "quote stays data" quoted {|list("raw-code", "host();")|};
  let escaped = java_compile {|(raw-code "  // punctuation !?+- /
\t\r\"\\\\n\q\n// tail") (def after 42)|} in
  check_contains "exact decoded payload and boundary" escaped
    ("\n" ^ decoded_payload ^ "\npublic static Object after = 42;")

let eval_rejection () =
  let message = "raw-code is supported only on the JS and Java targets" in
  let context = Backend_eval.Eval.create_context () in
  let eval source = Backend_eval.Eval.eval_all ~context (parse source) in
  ignore (eval "(def counter (atom 0))");
  List.iter
    (fun source ->
      Alcotest.check_raises "unsupported raw-code" (Backend_eval.Eval.Eval_error message) (fun () ->
          ignore (eval source)))
    [
      {|(raw-code "console.log('hello');")|};
      "(raw-code)";
      {|(raw-code "a" "b")|};
      "(raw-code (reset! counter 1))";
      "(raw-code (cond true))";
    ];
  Alcotest.(check bool) "arguments not evaluated" true (eval "(deref counter)" = [ Backend_eval.Eval.Int 0 ]);
  Alcotest.(check bool)
    "unreached branch" true
    (eval {|(if false (raw-code :invalid) "ok")|} = [ Backend_eval.Eval.String "ok" ]);
  Alcotest.(check bool)
    "quoted form is data" true
    (eval {|'(raw-code "x")|}
    = [ Backend_eval.Eval.List [ Backend_eval.Eval.Symbol "raw-code"; Backend_eval.Eval.String "x" ] ]);
  Alcotest.(check bool)
    "runner returns language error" true
    (Language_main.Runner.run ~target:"eval" {|(raw-code "x")|} = Error message)

let () =
  Alcotest.run "raw-code"
    [
      ( "js",
        [
          Alcotest.test_case "generation" `Quick js_generation;
          Alcotest.test_case "discarded branch statement blocks" `Quick js_discarded_branches;
          Alcotest.test_case "decoded payload" `Quick js_escapes;
          Alcotest.test_case "diagnostics" `Quick (diagnostics js_compile);
        ] );
      ( "java",
        [
          Alcotest.test_case "generation" `Quick java_generation;
          Alcotest.test_case "diagnostics" `Quick (diagnostics java_compile);
        ] );
      ("eval", [ Alcotest.test_case "unsupported without argument evaluation" `Quick eval_rejection ]);
    ]
