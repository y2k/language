open Frontend
module Pipeline = Backend_eval.Pipeline

let parse ?extensions source =
  match Pipeline.parse_and_desugar ?extensions source with Ok forms -> forms | Error message -> Alcotest.fail message

let check_forms label expected forms = Alcotest.(check (list string)) label expected (List.map show_sexpr forms)

let thread () =
  [
    (function
    | SList (meta, Paren, SAtom (_, "thread") :: args) -> Some (SList (meta, Paren, SAtom (meta, "->") :: args))
    | _ -> None);
  ]

let expands_to_builtin () =
  check_forms "recursive expansion" [ "(+ 42 1)" ] (parse ~extensions:[ thread ] "(thread 42 (+ 1))")

let configurations_are_independent () =
  let source = "(thread 42 (+ 1)) (-> 7 (+ 2))" in
  check_forms "enabled" [ "(+ 42 1)"; "(+ 7 2)" ] (parse ~extensions:[ thread ] source);
  check_forms "disabled" [ "(thread 42 (+ 1))"; "(+ 7 2)" ] (parse ~extensions:[] source);
  check_forms "default" [ "(thread 42 (+ 1))"; "(+ 7 2)" ] (parse source)

let preserves_handler_order () =
  let handler result = function
    | SList (meta, Paren, SAtom (_, "->") :: _) -> Some (SAtom (meta, result))
    | _ -> None
  in
  let extensions = [ (fun () -> [ handler "1"; handler "2" ]); (fun () -> [ handler "3" ]) ] in
  check_forms "extensions before builtins, first match wins" [ "1" ] (parse ~extensions "(-> 42 (+ 1))")

let language_cond_configuration () =
  let source = "(cond false 1 true 42) (-> 7 (+ 2))" in
  let expanded = [ "(if false 1 (if true 42 nil))"; "(+ 7 2)" ] in
  check_forms "default language extension" expanded (parse source);
  check_forms "disabled without fallback" [ "(cond false 1 true 42)"; "(+ 7 2)" ] (parse ~extensions:[] source);
  check_forms "default remains enabled" expanded (parse source)

let language_and_configuration () =
  let source = "(and) (and 42) (case 1 1 7 0)" in
  let forms = parse source in
  check_forms "default and" [ "true"; "42" ] (List.filteri (fun i _ -> i < 2) forms);
  let disabled = parse ~extensions:[] source in
  check_forms "disabled without fallback" [ "(and)"; "(and 42)" ] (List.filteri (fun i _ -> i < 2) disabled);
  Alcotest.(check bool) "remaining case still expands" true
    (Backend_eval.Eval.eval (List.nth disabled 2) = Backend_eval.Eval.Int 7);
  (match Frontend.parse_and_desugar "(and true 42)" with
  | Ok forms -> check_forms "frontend has no fallback" [ "(and true 42)" ] forms
  | Error message -> Alcotest.fail message);
  check_forms "default restored" [ "true" ] (parse "(and)")

let language_or_configuration () =
  let source = "(or) (or 42) (or false 7)" in
  let forms = parse source in
  Alcotest.(check bool) "default language or" true
    (Backend_eval.Eval.eval_all forms = [ Backend_eval.Eval.Nil; Backend_eval.Eval.Int 42; Backend_eval.Eval.Int 7 ]);
  check_forms "disabled without fallback" [ "(or)"; "(or 42)"; "(or false 7)" ] (parse ~extensions:[] source);
  (match Frontend.parse_and_desugar source with
  | Ok forms -> check_forms "frontend has no fallback" [ "(or)"; "(or 42)"; "(or false 7)" ] forms
  | Error message -> Alcotest.fail message);
  check_forms "default restored" [ "nil" ] (parse "(or)")

let with_open_expansion () =
  let expected =
    parse
      "(let [a (open-a)] (try (let [b (open-b a)] (try (do (use a b)) (finally (.close (cast java.lang.AutoCloseable \
       b))))) (finally (.close (cast java.lang.AutoCloseable a)))))"
    |> List.map show_sexpr
  in
  check_forms "nested resource protection" expected (parse "(with-open [a (open-a) b (open-b a)] (use a b))");
  check_forms "empty bindings" [ "(do 42)" ] (parse "(with-open [] 42)");
  check_forms "empty body" [ "nil" ] (parse "(with-open [])");
  match parse "(with-open [^java.io.Reader r (source)] (.read r))" with
  | [ SList (_, _, SAtom (_, "let*") :: SList (_, _, [ SAtom (meta, "r"); _ ]) :: _) ] ->
      Alcotest.(check (option string)) "binding type" (Some "java.io.Reader") meta.type_annotation
  | _ -> Alcotest.fail "expected annotated resource binding"

let language_with_open_configuration () =
  let source = "(with-open [])" in
  check_forms "default" [ "nil" ] (parse source);
  check_forms "disabled without fallback" [ "(with-open (list))" ] (parse ~extensions:[] source);
  (match Frontend.parse_and_desugar source with
  | Ok forms -> check_forms "frontend has no fallback" [ "(with-open (list))" ] forms
  | Error message -> Alcotest.fail message);
  Alcotest.(check bool) "case remains available" true
    (Backend_eval.Eval.eval_all (parse ~extensions:[] "(case 1 1 7 0)") = [ Backend_eval.Eval.Int 7 ]);
  check_forms "default restored" [ "nil" ] (parse source)

let fresh_factory_per_source () =
  let creations = ref 0 in
  let extension () =
    incr creations;
    let count = ref 0 in
    [
      (function
      | SList (meta, Paren, [ SAtom (_, "next") ]) ->
          incr count;
          Some (SAtom (meta, string_of_int !count))
      | _ -> None);
    ]
  in
  let extensions = [ extension ] in
  check_forms "first file" [ "1"; "2" ] (parse ~extensions "(next) (next)");
  check_forms "second file" [ "1"; "2" ] (parse ~extensions "(next) (next)");
  Alcotest.(check int) "one factory invocation per file" 2 !creations

let () =
  Alcotest.run "pipeline"
    [
      ( "extensions",
        [
          Alcotest.test_case "expands to builtin" `Quick expands_to_builtin;
          Alcotest.test_case "independent configurations" `Quick configurations_are_independent;
          Alcotest.test_case "handler order" `Quick preserves_handler_order;
          Alcotest.test_case "fresh factory per source" `Quick fresh_factory_per_source;
          Alcotest.test_case "language cond configuration" `Quick language_cond_configuration;
          Alcotest.test_case "language and configuration" `Quick language_and_configuration;
          Alcotest.test_case "language or configuration" `Quick language_or_configuration;
          Alcotest.test_case "with-open expansion" `Quick with_open_expansion;
          Alcotest.test_case "language with-open configuration" `Quick language_with_open_configuration;
        ] );
    ]
