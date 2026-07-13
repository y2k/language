let eval input =
  match Frontend.parse_and_desugar input with
  | Ok sexprs -> Backend_eval.Eval.eval_all sexprs
  | Error message -> Alcotest.fail message

let eval_with_context context input =
  match Frontend.parse_and_desugar input with
  | Ok sexprs -> Backend_eval.Eval.eval_all ~context sexprs
  | Error message -> Alcotest.fail message

let last_symbol input =
  match List.rev (eval input) with
  | Backend_eval.Eval.Symbol value :: _ -> value
  | _ -> Alcotest.fail "expected symbol result"

let compiler_ns_returns_nil () = Alcotest.(check string) "result" "nil" (last_symbol {|(compiler/ns "app.main" () ())|})

let def_and_lookup_use_current_namespace () =
  Alcotest.(check string) "result" "1" (last_symbol {|(compiler/ns "a" () ()) (def x 1) x|})

let lookup_uses_only_current_namespace () =
  Alcotest.check_raises "not found" (Backend_eval.Eval.Eval_error "symbol not found: x") (fun () ->
      ignore (eval {|(compiler/ns "a" () ()) (def x 1) (compiler/ns "b" () ()) x|}))

let context_keeps_current_namespace () =
  let context = Backend_eval.Eval.create_context () in
  ignore (eval_with_context context {|(compiler/ns "a" () ()) (def x 1)|});
  match List.rev (eval_with_context context {|x|}) with
  | Backend_eval.Eval.Symbol value :: _ -> Alcotest.(check string) "result" "1" value
  | _ -> Alcotest.fail "expected symbol result"

let qualified_lookup_uses_namespace () =
  Alcotest.(check string) "result" "1" (last_symbol {|(compiler/ns "a" () ()) (def x 1) (compiler/ns "b" () ()) a/x|})

let qualified_lookup_uses_alias () =
  Alcotest.(check string)
    "result" "1"
    (last_symbol {|(compiler/ns "a" () ()) (def x 1) (compiler/ns "b" (("a" "aa")) ()) aa/x|})

let get_reads_list_by_index () =
  Alcotest.(check string) "result" "20 nil" (last_symbol {|(str (get [10 20 30] 1) " " (get [10] 2))|})

let cast_is_no_op () = Alcotest.(check string) "result" "ab" (last_symbol {|(cast java.util.List (str "a" "b"))|})

let let_binds_nested_patterns () =
  Alcotest.(check string)
    "result" "a-b-c-Ada-nil"
    (last_symbol
       {|(let [[a [b c] {:name n :missing m}] (list "a" (list "b" "c") (hash-map "name" "Ada"))] (str a "-" b "-" c "-" n "-" m))|})

let fn_binds_nested_patterns () =
  Alcotest.(check string)
    "result" "a-b-nil-Ada-first-nil"
    (last_symbol
       {|(let [f (fn* ((list a (list b c)) (hash-map "name" n "tags" (list tag) "missing" m)) (str a "-" b "-" c "-" n "-" tag "-" m))] (f (list "a" (list "b") "ignored") (hash-map "name" "Ada" "tags" (list "first"))))|})

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
          Alcotest.test_case "let binds nested patterns" `Quick let_binds_nested_patterns;
          Alcotest.test_case "fn binds nested patterns" `Quick fn_binds_nested_patterns;
        ] );
    ]
