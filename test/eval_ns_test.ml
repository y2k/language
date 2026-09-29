let eval input =
  match Frontend.parse_and_desugar input with
  | Ok sexprs -> Backend_eval.Eval.eval_all sexprs
  | Error message -> Alcotest.fail message

let eval_with_context context input =
  match Frontend.parse_and_desugar input with
  | Ok sexprs -> Backend_eval.Eval.eval_all ~context sexprs
  | Error message -> Alcotest.fail message

let last_string input =
  match List.rev (eval input) with
  | Backend_eval.Eval.String value :: _ -> value
  | _ -> Alcotest.fail "expected string result"

let last_int input =
  match List.rev (eval input) with
  | Backend_eval.Eval.Int value :: _ -> value
  | _ -> Alcotest.fail "expected integer result"

let last_boolean input =
  match List.rev (eval input) with
  | Backend_eval.Eval.Bool value :: _ -> value
  | _ -> Alcotest.failf "expected boolean result: %s" input

let check_nil input =
  match eval input with [ Backend_eval.Eval.Nil ] -> () | _ -> Alcotest.failf "expected Nil: %s" input

let compiler_ns_returns_nil () = check_nil {|(compiler/ns "app.main" () ())|}

let def_and_lookup_use_current_namespace () =
  Alcotest.(check int) "result" 1 (last_int {|(compiler/ns "a" () ()) (def x 1) x|})

let lookup_uses_only_current_namespace () =
  Alcotest.check_raises "not found" (Backend_eval.Eval.Eval_error "symbol not found: x") (fun () ->
      ignore (eval {|(compiler/ns "a" () ()) (def x 1) (compiler/ns "b" () ()) x|}))

let context_keeps_current_namespace () =
  let context = Backend_eval.Eval.create_context () in
  ignore (eval_with_context context {|(compiler/ns "a" () ()) (def x 1)|});
  match List.rev (eval_with_context context {|x|}) with
  | Backend_eval.Eval.Int value :: _ -> Alcotest.(check int) "result" 1 value
  | _ -> Alcotest.fail "expected integer result"

let qualified_lookup_uses_namespace () =
  Alcotest.(check int) "result" 1 (last_int {|(compiler/ns "a" () ()) (def x 1) (compiler/ns "b" () ()) a/x|})

let qualified_lookup_uses_alias () =
  Alcotest.(check int)
    "result" 1
    (last_int {|(compiler/ns "a" () ()) (def x 1) (compiler/ns "b" (("a" "aa")) ()) aa/x|})

let get_reads_list_by_index () =
  Alcotest.(check string) "result" "20 nil" (last_string {|(str (get [10 20 30] 1) " " (get [10] 2))|})

let cast_is_no_op () = Alcotest.(check string) "result" "ab" (last_string {|(cast java.util.List (str "a" "b"))|})
let assert_returns_true () = Alcotest.(check bool) "result" true (last_boolean {|(assert "ok")|})

let assert_rejects_false () =
  Alcotest.check_raises "assertion failed" (Backend_eval.Eval.Eval_error "assertion failed") (fun () ->
      ignore (eval {|(assert false)|}))

let assert_rejects_nil () =
  Alcotest.check_raises "assertion failed" (Backend_eval.Eval.Eval_error "assertion failed") (fun () ->
      ignore (eval {|(assert nil)|}))

let let_binds_nested_patterns () =
  Alcotest.(check string)
    "result" "a-b-c-Ada-nil"
    (last_string
       {|(let [[a [b c] {:name n :missing m}] (list "a" (list "b" "c") (hash-map "name" "Ada"))] (str a "-" b "-" c "-" n "-" m))|})

let fn_binds_nested_patterns () =
  Alcotest.(check string)
    "result" "a-b-nil-Ada-first-nil"
    (last_string
       {|(let [f (fn* ((list a (list b c)) (hash-map "name" n "tags" (list tag) "missing" m)) (str a "-" b "-" c "-" n "-" tag "-" m))] (f (list "a" (list "b") "ignored") (hash-map "name" "Ada" "tags" (list "first"))))|})

let nil_literals_are_not_text () =
  List.iter check_nil [ "nil"; "'nil"; "(quote nil)"; {|(get '(nil "nil") 0)|} ];
  List.iter
    (fun input -> Alcotest.(check string) input "nil" (last_string input))
    [ {|"nil"|}; {|'"nil"|}; {|(get '(nil "nil") 1)|} ]

let nil_sources_are_not_text () =
  List.iter check_nil
    [
      "(if false 1)";
      {|(compiler/ns "app" () ())|};
      "(deps {})";
      "(get {} :missing)";
      "(get [] 0)";
      "(get nil :x)";
      "(get-in {:x nil} [:x :y])";
      "(get-in {} [:x :y])";
      "(let [[x] []] x)";
      "(let [{:x x} {}] x)";
      "((fn [[x]] x) [])";
      "((fn [{:x x}] x) {})";
      "((fn [x] x) nil)";
      "(deref (atom nil))";
      "(let [a (atom 1)] (reset! a nil) (deref a))";
      "(let [a (atom 1)] (swap! a (fn [x] nil)) (deref a))";
    ]

let nil_equality () =
  List.iter
    (fun (input, expected) -> Alcotest.(check string) input expected (string_of_bool (last_boolean input)))
    [
      ("(= nil nil)", "true");
      ({|(= nil "nil")|}, "false");
      ({|(= "nil" nil)|}, "false");
      ({|(not= nil "nil")|}, "true");
      ("(= [nil] [nil])", "true");
      ({|(= [nil] ["nil"])|}, "false");
      ("(= {:x nil} {:x nil})", "true");
      ({|(= {:x nil} {:x "nil"})|}, "false");
      ("(= nil false)", "false");
      ("(= nil [])", "false");
    ]

let nil_truthiness () =
  List.iter
    (fun (input, expected) -> Alcotest.(check string) input expected (last_string input))
    [
      ("(str (if nil 1 2))", "2");
      ({|(str (if "nil" 1 2))|}, "1");
      ("(str (not nil))", "true");
      ({|(str (not "nil"))|}, "false");
      ({|(str (assert "nil"))|}, "true");
      ({|(str (if (str nil) 1 2))|}, "1");
      ({|(str (if "false" 1 2))|}, "1");
      ({|(str (not "false"))|}, "false");
    ];
  match eval "(do)" with [ Backend_eval.Eval.List [] ] -> () | _ -> Alcotest.fail "empty do must remain an empty list"

let nil_map_keys () =
  List.iter
    (fun input -> Alcotest.(check string) input "1 2" (last_string input))
    [
      {|(let [m (hash-map nil 1 "nil" 2)] (str (get m nil) " " (get m "nil")))|};
      {|(let [{nil a "nil" b} (hash-map nil 1 "nil" 2)] (str a " " b))|};
      {|((fn [{nil a "nil" b}] (str a " " b)) (hash-map nil 1 "nil" 2))|};
    ]

let nil_text_is_not_a_collection () =
  List.iter
    (fun input ->
      match eval input with
      | exception Backend_eval.Eval.Eval_error _ -> ()
      | _ -> Alcotest.failf "expected collection error: %s" input)
    [ {|(get "nil" :x)|}; {|(get-in {:x "nil"} [:x :y])|} ]

let nil_runner_output () =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check (result string string)) input (Ok expected) (Language_main.Runner.run ~target:"eval" input))
    [
      ("nil", "nil");
      ("'nil", "nil");
      ("(if false 1)", "nil");
      ({|"nil"|}, "nil");
      ({|"hello"|}, "hello");
      ("(str nil)", "nil");
      ("(str [nil])", "(nil)");
      ({|(str {"x" nil})|}, "{x nil}");
      ("[nil]", "");
      ("{:x nil}", "");
      ("(fn [] nil)", "");
      ("(atom nil)", "");
      ("", "");
    ];
  Alcotest.(check (result string string))
    "nil is not callable"
    (Error "first list item is not a function at line 1, column 2: nil evaluated to nil")
    (Language_main.Runner.run ~target:"eval" "(nil)")

let boolean_literals_are_not_text () =
  List.iter
    (fun input -> Alcotest.(check bool) input false (last_boolean input))
    [ {|(= false "false")|}; {|(= true "true")|}; {|(= 'false "false")|}; {|(= 'true "true")|} ];
  List.iter
    (fun (input, expected) -> Alcotest.(check string) input expected (last_string input))
    [
      ({|"false"|}, "false");
      ({|"true"|}, "true");
      ({|'"false"|}, "false");
      ({|'"true"|}, "true");
      (":false", "false");
      ("(str false)", "false");
    ]

let boolean_sources () =
  List.iter
    (fun (input, expected) -> Alcotest.(check bool) input expected (last_boolean input))
    [
      ("false", false);
      ("true", true);
      ("'false", false);
      ("'true", true);
      ("(get '(false true) 0)", false);
      ("(get '(false true) 1)", true);
      ("(=)", true);
      ("(= false)", true);
      ("(= false false)", true);
      ("(= true false)", false);
      ("(not=)", false);
      ("(not= false true)", true);
      ("(not true)", false);
      ("(not false)", true);
      ("(not nil)", true);
      ("(vector? [])", true);
      ("(vector? false)", false);
      ("(< 1 2)", true);
      ("(> 1 2)", false);
      ("(<= 2 2)", true);
      ("(>= 1 2)", false);
      ({|(assert "false")|}, true);
      ("(deref (atom false))", false);
      ("(reset! (atom true) false)", false);
      ("(swap! (atom true) (fn [x] (not x)))", false);
    ]

let boolean_runner_output () =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check (result string string)) input (Ok expected) (Language_main.Runner.run ~target:"eval" input))
    [
      ("false", "false");
      ("true", "true");
      ("'false", "false");
      ("(= 1 1)", "true");
      ("(not true)", "false");
      ("(str false)", "false");
      ("(if (str false) 1 2)", "1");
      ("(str [false true])", "(false true)");
      ({|(str {"enabled" false})|}, "{enabled false}");
      ("[false]", "");
      ("{:x false}", "");
      ("(fn [] false)", "");
      ("(atom false)", "");
    ];
  Alcotest.(check (result string string))
    "boolean is not callable"
    (Error "first list item is not a function at line 1, column 2: false evaluated to boolean false")
    (Language_main.Runner.run ~target:"eval" "(false)")

let scalar_equality () =
  List.iter
    (fun (input, expected) -> Alcotest.(check bool) input expected (last_boolean input))
    [
      ({|(= 42 "42")|}, false);
      ({|(= 'foo "foo")|}, false);
      ("(= 1 1.0)", true);
      ("(= 1.0 1)", true);
      ("(= 1 1.5)", false);
      ("(= 0.1 0.10000000000000002)", false);
      ("(= 9007199254740993 9007199254740992.0)", false);
      ("(= 9007199254740992 9007199254740992.0)", true);
      (Printf.sprintf "(= %d %.1f)" max_int (float_of_int max_int), false);
      (Printf.sprintf "(= %d %.1f)" min_int (float_of_int min_int), true);
      (Printf.sprintf "(= %d %.1f)" (min_int + 1) (float_of_int min_int), false);
    ]

let strict_scalar_consumers () =
  List.iter
    (fun input ->
      match eval input with
      | exception Backend_eval.Eval.Eval_error _ -> ()
      | _ -> Alcotest.failf "expected scalar type error: %s" input)
    [
      {|(+ "1" 2)|};
      {|(- "1" 2)|};
      {|(* "1" 2)|};
      {|(/ "2" 1)|};
      {|(+ 'foo 2)|};
      {|(get [10] "0")|};
      "(get [10] 0.0)";
      "(get [10] 'index)";
      {|(drop "1" [10])|};
      "(drop 1.0 [10])";
      "(drop 'count [10])";
      "(/ 2.0 1)";
      "(/ 2 1.0)";
      "(< 1.0 2)";
      {|(> "2" 1)|};
      "(<= 'x 1)";
      "(>= 1 1.0)";
    ];
  List.iter
    (fun input ->
      Alcotest.check_raises input (Backend_eval.Eval.Eval_error "slurp expects one path") (fun () ->
          ignore (eval input)))
    [ "(slurp 'file)"; "(slurp 42)"; "(slurp 1.0)" ]

let scalar_constructors () =
  let evaluate = eval in
  let open Backend_eval.Eval in
  List.iter
    (fun (input, expected) ->
      match List.rev (evaluate input) with
      | actual :: _ -> Alcotest.(check bool) ("constructor: " ^ input) true (actual = expected)
      | [] -> Alcotest.fail "missing scalar")
    [
      ("42", Int 42);
      ("'42", Int 42);
      ("0.25", Float 0.25);
      ("'0.25", Float 0.25);
      ("1.00", Float 1.);
      ({|"42"|}, String "42");
      ({|'"42"|}, String "42");
      ("'foo", Symbol "foo");
      (":name", String "name");
      ("'nil", Nil);
      ("'false", Bool false);
      ("(get '(42 0.25 foo) 2)", Symbol "foo");
      ("(count [1 2])", Int 2);
      ("(count {:x 1})", Int 1);
      ("(+)", Int 0);
      ("(*)", Int 1);
      ("(/ 7 2)", Int 3);
      ("(+ 0.5 0.5)", Int 1);
      ("(- 1.5 0.5)", Int 1);
      ("(* 2 0.5)", Int 1);
      ("(* -0.5 0)", Int 0);
      ("(- 1.0)", Int 1);
      ("(+ -2147483648.0 0)", Int (-2147483648));
      ("(+ 2147483647.0 0)", Int 2147483647);
      ("(+ 2147483648.0 0)", Float 2147483648.);
      ("(* (+ 0.1 0.2) 10)", Float 3.0000000000000004);
      ("(str 42)", String "42");
      ("(str 'foo)", String "foo");
      ("(let [1 9] 1)", Int 9);
      ("(let [foo 42] 'foo)", Symbol "foo");
    ];
  let path = Filename.temp_file "language-scalar-" ".txt" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      Out_channel.with_open_text path (fun channel -> output_string channel "42\ntext\n");
      Alcotest.(check string) "slurp returns String" "42\ntext\n" (last_string (Printf.sprintf "(slurp %S)" path)))

let scalar_runner_and_diagnostics () =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check (result string string)) input (Ok expected) (Language_main.Runner.run ~target:"eval" input))
    [ ("42", "42"); ("0.25", "0.25"); ("1.00", "1"); ({|"text"|}, "text"); ("'foo", "foo") ];
  List.iter
    (fun (atom, category) ->
      let input = "(" ^ atom ^ ")" in
      Alcotest.(check (result string string))
        input
        (Error ("first list item is not a function at line 1, column 2: " ^ atom ^ " evaluated to " ^ category))
        (Language_main.Runner.run ~target:"eval" input))
    [ ("42", "integer 42"); ("0.25", "float 0.25"); ("\"text\"", "string \"text\"") ];
  Alcotest.(check string)
    "symbol diagnostic category" "symbol \"foo\""
    (Backend_eval.Eval.value_text (Backend_eval.Eval.Symbol "foo"))

let () =
  Alcotest.run "eval ns"
    [
      ( "compiler/ns",
        [
          Alcotest.test_case "returns nil" `Quick compiler_ns_returns_nil;
          Alcotest.test_case "def and lookup use current namespace" `Quick def_and_lookup_use_current_namespace;
          Alcotest.test_case "lookup uses only current namespace" `Quick lookup_uses_only_current_namespace;
          Alcotest.test_case "context keeps current namespace" `Quick context_keeps_current_namespace;
          Alcotest.test_case "qualified lookup uses namespace" `Quick qualified_lookup_uses_namespace;
          Alcotest.test_case "qualified lookup uses alias" `Quick qualified_lookup_uses_alias;
        ] );
      ( "stdlib",
        [
          Alcotest.test_case "get reads list by index" `Quick get_reads_list_by_index;
          Alcotest.test_case "cast is no-op" `Quick cast_is_no_op;
          Alcotest.test_case "assert returns true" `Quick assert_returns_true;
          Alcotest.test_case "assert rejects false" `Quick assert_rejects_false;
          Alcotest.test_case "assert rejects nil" `Quick assert_rejects_nil;
          Alcotest.test_case "let binds nested patterns" `Quick let_binds_nested_patterns;
          Alcotest.test_case "fn binds nested patterns" `Quick fn_binds_nested_patterns;
        ] );
      ( "nil",
        [
          Alcotest.test_case "literals and quote differ from text" `Quick nil_literals_are_not_text;
          Alcotest.test_case "missing values differ from text" `Quick nil_sources_are_not_text;
          Alcotest.test_case "equality" `Quick nil_equality;
          Alcotest.test_case "truthiness" `Quick nil_truthiness;
          Alcotest.test_case "distinct map keys" `Quick nil_map_keys;
          Alcotest.test_case "text is not a collection" `Quick nil_text_is_not_a_collection;
          Alcotest.test_case "runner output" `Quick nil_runner_output;
        ] );
      ( "boolean",
        [
          Alcotest.test_case "literals and quote differ from text" `Quick boolean_literals_are_not_text;
          Alcotest.test_case "sources return booleans" `Quick boolean_sources;
          Alcotest.test_case "runner output" `Quick boolean_runner_output;
        ] );
      ( "scalars",
        [
          Alcotest.test_case "numeric equality preserves integer precision" `Quick scalar_equality;
          Alcotest.test_case "strict consumers reject textual numbers and float indexes" `Quick strict_scalar_consumers;
          Alcotest.test_case "scalar constructors and producers" `Quick scalar_constructors;
          Alcotest.test_case "scalar runner and diagnostics" `Quick scalar_runner_and_diagnostics;
        ] );
    ]
