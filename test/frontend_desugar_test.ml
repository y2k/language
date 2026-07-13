open Frontend

let parse_one input =
  match Frontend.parse_and_desugar input with
  | Ok [ sexpr ] -> sexpr
  | Ok sexprs -> Alcotest.failf "expected one sexpr, got %d" (List.length sexprs)
  | Error message -> Alcotest.fail message

let check_desugar name input expected = Alcotest.(check string) name expected (Frontend.show_sexpr (parse_one input))
let method_call_shorthand () = check_desugar "method call shorthand" "(.foo obj 1 2)" "(. obj foo 1 2)"
let explicit_method_call_unchanged () = check_desugar "explicit method call" "(. obj foo 1 2)" "(. obj foo 1 2)"
let constructor_shorthand () = check_desugar "constructor shorthand" "(LocalDate. 2024 1 2)" "(new LocalDate 2024 1 2)"
let let_sequential_pattern () = check_desugar "let sequential pattern" "(let [[a b] xs] a)" "(let* ((list a b) xs) a)"

let multiple_import_vectors () =
  check_desugar "multiple import vectors" "(ns app.main (:import [java.time LocalDate] [java.util UUID]))"
    {|(compiler/ns
 "app.main"
 ()
 (("LocalDate" "java.time.LocalDate") ("UUID" "java.util.UUID")))|}

let let_associative_pattern () =
  check_desugar "let associative pattern" "(let [{:name n} user] n)" "(let* ((hash-map \"name\" n) user) n)"

let rec function_parts = function
  | SList
      ( _,
        _,
        SAtom (_, "fn*") :: SList (_, _, params) :: SList (_, _, SAtom (_, "let*") :: SList (_, _, bindings) :: _) :: _
      ) ->
      (params, bindings)
  | SList (_, _, [ SAtom (_, "def"); _; fn ]) -> function_parts fn
  | _ -> Alcotest.fail "expected function with parameter casts"

let parameter_casts params bindings =
  let rec loop params bindings =
    match (params, bindings) with
    | ( SAtom (_, argument) :: params,
        SAtom (_, name)
        :: SList (_, _, [ SAtom (_, "cast"); SAtom (_, type_name); SAtom (_, cast_argument) ])
        :: bindings ) ->
        (argument, name, type_name, cast_argument) :: loop params bindings
    | [], [] -> []
    | _ -> Alcotest.fail "expected parameter cast bindings"
  in
  loop params bindings

let check_parameter_casts input expected =
  let params, bindings = parse_one input |> function_parts in
  let casts = parameter_casts params bindings in
  Alcotest.(check (list (pair string string)))
    "parameter casts" expected
    (List.map (fun (_, name, type_name, _) -> (name, type_name)) casts);
  List.iter
    (fun (argument, name, _, cast_argument) ->
      Alcotest.(check bool) (name ^ " is fresh") true (argument <> name);
      Alcotest.(check string) (name ^ " cast argument") argument cast_argument)
    casts

let annotated_fn_parameters () =
  check_parameter_casts "(fn [^String x ^int y] (str x y))" [ ("x", "String"); ("y", "int") ]

let annotated_defn_parameter () =
  check_parameter_casts "(defn size [^java.util.List xs] (.size xs))" [ ("xs", "java.util.List") ]

let () =
  Alcotest.run "frontend desugar"
    [
      ( "interop",
        [
          Alcotest.test_case "method call shorthand" `Quick method_call_shorthand;
          Alcotest.test_case "explicit method call unchanged" `Quick explicit_method_call_unchanged;
          Alcotest.test_case "constructor shorthand" `Quick constructor_shorthand;
          Alcotest.test_case "let sequential pattern" `Quick let_sequential_pattern;
          Alcotest.test_case "multiple import vectors" `Quick multiple_import_vectors;
          Alcotest.test_case "let associative pattern" `Quick let_associative_pattern;
          Alcotest.test_case "annotated fn parameters" `Quick annotated_fn_parameters;
          Alcotest.test_case "annotated defn parameter" `Quick annotated_defn_parameter;
        ] );
    ]
