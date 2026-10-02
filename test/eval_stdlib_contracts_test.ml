module Eval = Backend_eval.Eval
module Stdlib_eval = Backend_eval.Eval_stdlib
open Backend_eval.Eval_types

let eval ?(context = Eval.create_context ()) source =
  match Frontend.parse_and_desugar source with
  | Error message -> Alcotest.fail message
  | Ok forms -> (
      match List.rev (Eval.eval_all ~context forms) with value :: _ -> value | [] -> Alcotest.fail "expected a result")

let value = Alcotest.testable (fun out v -> Format.pp_print_string out (Stdlib_eval.to_string v)) ( = )
let check source expected = Alcotest.check value source expected (eval source)

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let check_message operation reason message =
  Alcotest.(check bool) ("operation in: " ^ message) true (contains message operation);
  Alcotest.(check bool) ("reason in: " ^ message) true (contains message reason)

let error operation reason source =
  match eval source with
  | exception Eval_error message -> check_message operation reason message
  | _ -> Alcotest.failf "expected Eval_error: %s" source

(* Each row covers an actual env binding, including its valid boundary arities.
   Empty invalid lists mean the binding accepts all types and all arities. *)
let matrix path =
  [
    ( "re-pattern",
      [ ({|(str (re-pattern ""))|}, String "#<regex>"); ({|(str (re-pattern :x))|}, String "#<regex>") ],
      [
        "(re-pattern)";
        {|(re-pattern "x" "flags")|};
        "(re-pattern 'x)";
        "(re-pattern nil)";
        "(re-pattern false)";
        "(re-pattern 1)";
        "(re-pattern [])";
        {|(re-pattern (re-pattern "x"))|};
      ] );
    ( "re-find",
      [
        ({|(re-find (re-pattern "x") "x")|}, String "x");
        ({|(re-find (re-pattern :x) :x)|}, String "x");
        ({|(re-find (re-pattern "x") "")|}, Nil);
      ],
      [
        "(re-find)";
        {|(re-find (re-pattern "x"))|};
        {|(re-find (re-pattern "x") "x" "x")|};
        {|(re-find "x" "")|};
        {|(re-find nil "")|};
        {|(re-find (re-pattern "x") nil)|};
        {|(re-find (re-pattern "x") 'x)|};
        {|(re-find (re-pattern "x") false)|};
        {|(re-find (re-pattern "x") 1)|};
      ] );
    ( "re-replace",
      [
        ({|(re-replace "xx" (re-pattern "x") "y")|}, String "yy"); ({|(re-replace :x (re-pattern :x) :y)|}, String "y");
      ],
      [
        "(re-replace)";
        {|(re-replace "x" (re-pattern "x"))|};
        {|(re-replace "x" (re-pattern "x") "y" "z")|};
        {|(re-replace nil (re-pattern "x") "y")|};
        {|(re-replace 'x (re-pattern "x") "y")|};
        {|(re-replace false (re-pattern "x") "y")|};
        {|(re-replace 1 (re-pattern "x") "y")|};
        {|(re-replace "" "x" "y")|};
        {|(re-replace "" nil "y")|};
        {|(re-replace "" (re-pattern "x") nil)|};
        {|(re-replace "" (re-pattern "x") 'y)|};
        {|(re-replace "" (re-pattern "x") false)|};
        {|(re-replace "" (re-pattern "x") 1)|};
        {|(re-replace "" (re-pattern "x") (fn [x] x))|};
      ] );
    ("list", [ ("(list)", List []); ("(list nil false 1 \"1\")", List [ Nil; Bool false; Int 1; String "1" ]) ], []);
    ( "hash-map",
      [ ("(hash-map)", HashMap []); ("(hash-map 1 false 1.0 nil)", HashMap [ (Int 1, Bool false); (Float 1., Nil) ]) ],
      [ "(hash-map 1)" ] );
    ("str", [ ("(str)", String ""); ("(str nil false 1 'x)", String "nilfalse1x") ], []);
    ( "concat",
      [ ("(concat)", List []); ("(concat [] [1] [false])", List [ Int 1; Bool false ]) ],
      [ "(concat [] {})"; "(concat nil)" ] );
    ("=", [ ("(=)", Bool true); ("(= 1)", Bool true); ("(= 1 1.0 1)", Bool true); ("(= 1 1.0 \"1\")", Bool false) ], []);
    ( "not=",
      [
        ("(not=)", Bool false);
        ("(not= 1)", Bool false);
        ("(not= 1 1.0 1)", Bool false);
        ("(not= 1 1.0 \"1\")", Bool true);
      ],
      [] );
    ( "not",
      [ ("(not nil)", Bool true); ("(not false)", Bool true); ("(not \"false\")", Bool false) ],
      [ "(not)"; "(not 1 2)" ] );
    ("assert", [ ("(assert \"false\")", Bool true); ("(assert [])", Bool true) ], [ "(assert)"; "(assert 1 2)" ]);
    ("vector?", [ ("(vector? [])", Bool true); ("(vector? nil)", Bool false) ], [ "(vector?)"; "(vector? [] [])" ]);
    ("atom", [ ("(deref (atom nil))", Nil); ("(deref (atom false))", Bool false) ], [ "(atom)"; "(atom 1 2)" ]);
    ("deref", [ ("(deref (atom 42))", Int 42) ], [ "(deref)"; "(deref (atom 1) 2)"; "(deref false)" ]);
    ( "reset!",
      [ ("(reset! (atom 1) false)", Bool false) ],
      [ "(reset! (atom 1))"; "(reset! (atom 1) 2 3)"; "(reset! nil 2)" ] );
    ( "swap!",
      [ ("(swap! (atom 1) (fn [x] (+ x 1)))", Int 2) ],
      [ "(swap! (atom 1))"; "(swap! (atom 1) (fn [x] x) 2)"; "(swap! nil (fn [x] x))" ] );
    ( "count",
      [ ("(count [])", Int 0); ("(count (hash-map 1 2 1 3))", Int 2) ],
      [ "(count)"; "(count [] [])"; "(count \"abc\")" ] );
    ( "slurp",
      [ ("(slurp \"" ^ path ^ "\")", String "first\nsecond\n") ],
      [ "(slurp)"; "(slurp \"a\" \"b\")"; "(slurp 42)"; "(slurp 'file)" ] );
    ( "get",
      [
        ("(get [10] 5)", Nil);
        ("(get [] 0)", Nil);
        ("(get nil -1)", Nil);
        ("(get nil 0.0)", Nil);
        ("(get {:x false} :x)", Bool false);
      ],
      [ "(get {})"; "(get {} :x nil)"; "(get 1 :x)"; "(get [10] \"0\")"; "(get [10] 0.0)"; "(get [10] 'x)" ] );
    ( "get-in",
      [
        ("(get-in false [])", Bool false);
        ("(get-in {:x [42]} [:x 0])", Int 42);
        ("(get-in nil [-1 \"x\"])", Nil);
        ("(get-in {} [:x -1])", Nil);
      ],
      [ "(get-in {})"; "(get-in {} [] nil)"; "(get-in nil nil)"; "(get-in nil {})"; "(get-in nil 0)" ] );
    ( "map",
      [ ("(map (fn [x] x) [])", List []); ("(map (fn [x] (+ x 1)) [1 2])", List [ Int 2; Int 3 ]) ],
      [ "(map (fn [x] x))"; "(map (fn [x] x) [] [])"; "(map (fn [x] x) {})" ] );
    ( "run!",
      [ ("(run! (fn [x] x) [])", Nil); ("(run! (fn [x] x) [1 2])", Nil) ],
      [
        "(run!)";
        "(run! (fn [x] x))";
        "(run! (fn [x] x) [] nil)";
        "(run! (fn [x] x) {})";
        "(run! (fn [x] x) nil)";
      ] );
    ( "reduce",
      [
        ("(reduce + [2])", Int 2);
        ("(reduce + [1 2 3])", Int 6);
        ("(reduce + 10 [])", Int 10);
        ("(reduce + 10 [1 2])", Int 13);
        ("(reduce (fn [acc [k v]] (+ acc v)) 0 {:x 2 :y 3})", Int 5);
      ],
      [
        "(reduce)";
        "(reduce +)";
        "(reduce + 0 [] [])";
        "(reduce + [])";
        "(reduce + {})";
        "(reduce + 1)";
        "(reduce + 0 nil)";
      ] );
    ( "drop",
      [
        ("(drop -1 [10])", List [ Int 10 ]);
        ("(drop 0 [10])", List [ Int 10 ]);
        ("(drop 5 [10])", List []);
        ("(drop 1 [])", List []);
      ],
      [ "(drop 1)"; "(drop 1 [] [])"; "(drop 1.0 [])"; "(drop \"1\" [])"; "(drop 1 {})" ] );
    ("+", [ ("(+)", Int 0); ("(+ 2)", Int 2); ("(+ 1 0.25 2)", Float 3.25) ], [ "(+ \"1\" 2)"; "(+ 1 'x)" ]);
    ("-", [ ("(- 2)", Int 2); ("(- 2 0.25 1)", Float 0.75) ], [ "(-)"; "(- 1 \"1\")" ]);
    ("*", [ ("(*)", Int 1); ("(* 2)", Int 2); ("(* 2 0.25 3)", Float 1.5) ], [ "(* true 2)"; "(* 2 'x)" ]);
    ( "/",
      [ ("(/ 0)", Int 0); ("(/ 2)", Int 2); ("(/ 7 2)", Int 3); ("(/ -7 2)", Int (-3)); ("(/ 20 2 2)", Int 5) ],
      [ "(/)"; "(/ 1.0 1)"; "(/ 2 \"1\")" ] );
    (">", [ ("(> 2 1)", Bool true); ("(> 1 1)", Bool false) ], [ "(> 1)"; "(> 1 2 3)"; "(> 1.0 0)" ]);
    ("<", [ ("(< 1 2)", Bool true); ("(< 1 1)", Bool false) ], [ "(< 1)"; "(< 1 2 3)"; "(< \"1\" 2)" ]);
    (">=", [ ("(>= 1 1)", Bool true); ("(>= 0 1)", Bool false) ], [ "(>= 1)"; "(>= 1 2 3)"; "(>= 1 'x)" ]);
    ("<=", [ ("(<= 1 1)", Bool true); ("(<= 2 1)", Bool false) ], [ "(<= 1)"; "(<= 1 2 3)"; "(<= false 1)" ]);
  ]

let matrix_covers_env () =
  let names = List.map (fun (name, _, _) -> name) (matrix "unused") in
  Alcotest.(check (list string))
    "all bindings covered"
    (List.sort String.compare (List.map fst Stdlib_eval.env))
    (List.sort String.compare names)

let with_file f =
  let path = Filename.temp_file "language-contracts-" ".txt" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      Out_channel.with_open_text path (fun channel -> output_string channel "first\nsecond\n");
      f path)

let valid_boundaries () =
  with_file (fun path ->
      List.iter (fun (_, cases, _) -> List.iter (fun (source, expected) -> check source expected) cases) (matrix path))

let invalid_arguments () =
  List.iter
    (fun (name, _, cases) -> List.iter (error name (if name = "hash-map" then "key/value" else "expects")) cases)
    (matrix "unused")

let invalid_callbacks =
  [
    ("map", "(map 42 [])");
    ("map", "(map 42 [1])");
    ("run!", "(run! 42 [])");
    ("run!", "(run! 42 [1])");
    ("reduce", "(reduce 42 0 [])");
    ("reduce", "(reduce 42 0 {})");
    ("reduce", "(reduce 42 [1])");
    ("reduce", "(reduce 42 {:x 1})");
    ("reduce", "(reduce 42 [1 2])");
    ("swap!", "(swap! (atom 1) 42)");
  ]

let callbacks_are_only_called_when_needed () =
  (* Wrong-arity closures must be accepted without calls; their bodies must not run. *)
  List.iter
    (fun (source, expected) -> check source expected)
    [
      ("(map (fn [] (assert false)) [])", List []);
      ("(run! (fn [a b] (assert false)) [])", Nil);
      ("(reduce (fn [] (assert false)) :init [])", String "init");
      ("(reduce (fn [] (assert false)) :init {})", String "init");
      ("(reduce (fn [] (assert false)) [42])", Int 42);
      ("(reduce (fn [] (assert false)) {:x 42})", List [ String "x"; Int 42 ]);
    ];
  List.iter
    (fun source ->
      Alcotest.check_raises source (Eval_error "wrong number of arguments") (fun () -> ignore (eval source)))
    [
      "(map (fn [] 1) [1])";
      "(run! (fn [a b] a) [1])";
      "(reduce (fn [] 1) [1 2])";
      "(reduce (fn [] 1) 0 [1])";
      "(swap! (atom 1) (fn [] 1))";
    ];
  List.iter
    (fun source -> Alcotest.check_raises source (Eval_error "assertion failed") (fun () -> ignore (eval source)))
    [
      "(map (fn [x] (assert false)) [1])";
      "(reduce (fn [a b] (assert false)) [1 2])";
      "(reduce (fn [a b] (assert false)) 0 [1])";
      "(swap! (atom 1) (fn [x] (assert false)))";
    ]

let swap_failure_preserves_callback_effects () =
  let context = Eval.create_context () in
  ignore (eval ~context "(def cell (atom 1)) (def effects (atom 0))");
  Alcotest.check_raises "callback failure" (Eval_error "assertion failed") (fun () ->
      ignore (eval ~context "(swap! cell (fn [x] (reset! cell 9) (reset! effects 1) (assert false)))"));
  Alcotest.check value "no final write or rollback" (Int 9) (eval ~context "(deref cell)");
  Alcotest.check value "external effect retained" (Int 1) (eval ~context "(deref effects)");
  (match eval ~context "(map (do (reset! effects 2) 42) (do (reset! effects 3) []))" with
  | exception Eval_error message -> check_message "map" "function" message
  | _ -> Alcotest.fail "expected callback rejection");
  Alcotest.check value "arguments evaluated eagerly" (Int 3) (eval ~context "(deref effects)")

let run_failure_preserves_callback_effects () =
  let context = Eval.create_context () in
  ignore (eval ~context "(def effects (atom \"\"))");
  Alcotest.check_raises "run! preserves callback error" (Eval_error "assertion failed") (fun () ->
      ignore
        (eval ~context
           {|(run! (fn [item]
              (swap! effects (fn [text] (str text item)))
              (if (= item 2) (assert false) "ignored")) [1 2 3])|}));
  Alcotest.check value "effects retained and third item skipped" (String "12") (eval ~context "(deref effects)")

let operation_errors =
  [
    ("get", "index", "(get [10] -1)");
    ("get", "index", "(get [] -1)");
    ("get", "index", "(get-in {:items [10]} [:items -1])");
    ("get", "index", "(get-in {:items [10]} [:items \"0\"])");
    ("get", "index", "(get-in {:items [10]} [:items 0.0])");
    ("/", "zero", "(/ 1 0)");
    ("/", "zero", "(/ 20 2 0)");
  ]

let exact_errors () =
  let nul_path = "bad\000path" in
  List.iter
    (fun (source, message) -> Alcotest.check_raises source (Eval_error message) (fun () -> ignore (eval source)))
    [
      ("(assert false)", "assertion failed");
      ("(assert nil)", "assertion failed");
      ("(hash-map :x)", "hash-map arguments must be key/value pairs");
      ("(slurp 42)", "slurp expects one path");
      ("(slurp \"" ^ nul_path ^ "\")", "slurp failed: " ^ nul_path);
    ];
  with_file (fun path ->
      let missing = path ^ "/missing" in
      Alcotest.check_raises "file API failure"
        (Eval_error ("slurp failed: " ^ missing))
        (fun () -> ignore (eval ("(slurp \"" ^ missing ^ "\")"))))

let runner_and_cli_errors () =
  with_file (fun path ->
      let cases =
        operation_errors
        @ [
            ("not", "expects", "(not)");
            ("count", "expects", "(count 42)");
            ("assert", "assertion failed", "(assert false)");
            ("hash-map", "key/value", "(hash-map :x)");
            ("slurp", "expects one path", "(slurp 42)");
            ("slurp", "failed", "(slurp \"" ^ path ^ "/missing\")");
            ("slurp", "failed", "(slurp \"bad\000path\")");
          ]
      in
      List.iter
        (fun (name, reason, source) ->
          let message =
            match Language_main.Runner.run ~target:"eval" source with
            | Error message ->
                check_message name reason message;
                message
            | Ok output -> Alcotest.failf "expected runner error, got %S" output
          in
          let program = "../bin/main.exe" in
          let channels = Unix.open_process_args_full program [| program; "--target"; "eval" |] (Unix.environment ()) in
          let stdout, stdin, stderr = channels in
          output_string stdin source;
          close_out stdin;
          let output = In_channel.input_all stdout in
          let error = In_channel.input_all stderr in
          let status = Unix.close_process_full channels in
          Alcotest.(check bool) "nonzero exit" true (match status with Unix.WEXITED n -> n <> 0 | _ -> false);
          Alcotest.(check string) "no success output" "" output;
          Alcotest.(check string) "CLI stderr preserves runner error" (message ^ "\n") error)
        cases)

let () =
  Alcotest.run "eval stdlib contracts"
    [
      ( "matrix",
        [
          Alcotest.test_case "covers every binding" `Quick matrix_covers_env;
          Alcotest.test_case "valid boundaries" `Quick valid_boundaries;
          Alcotest.test_case "invalid arguments" `Quick invalid_arguments;
        ] );
      ( "callbacks",
        List.map
          (fun (name, source) -> Alcotest.test_case source `Quick (fun () -> error name "function" source))
          invalid_callbacks
        @ [
            Alcotest.test_case "only invoke when needed and preserve errors" `Quick
              callbacks_are_only_called_when_needed;
            Alcotest.test_case "swap failure preserves callback effects" `Quick swap_failure_preserves_callback_effects;
            Alcotest.test_case "run failure preserves callback effects" `Quick run_failure_preserves_callback_effects;
          ] );
      ( "errors",
        List.map
          (fun (name, reason, source) -> Alcotest.test_case source `Quick (fun () -> error name reason source))
          operation_errors
        @ [
            Alcotest.test_case "exact messages and invalid paths" `Quick exact_errors;
            Alcotest.test_case "runner and CLI" `Quick runner_and_cli_errors;
          ] );
    ]
