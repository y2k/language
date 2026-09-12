open Frontend

let parse_one input =
  match Frontend.parse_and_desugar input with
  | Ok [ sexpr ] -> sexpr
  | Ok sexprs -> Alcotest.failf "expected one sexpr, got %d" (List.length sexprs)
  | Error message -> Alcotest.fail message

let check_desugar name input expected = Alcotest.(check string) name expected (Frontend.show_sexpr (parse_one input))

let string_escapes () =
  List.iter
    (fun (input, expected) ->
      match parse_one input with
      | SAtom (_, value) -> Alcotest.(check string) input expected value
      | _ -> Alcotest.fail "expected string atom")
    [
      ({|"a\nb"|}, "\"a\nb\"");
      ({|"a\"b"|}, "\"a\"b\"");
      ({|"a\\b"|}, "\"a\\b\"");
      ({|"a\tb"|}, "\"a\tb\"");
      ({|"a\rb"|}, "\"a\rb\"");
      ({|"\"\\\n\t\r"|}, "\"\"\\\n\t\r\"");
      ({|"\\n"|}, "\"\\n\"");
      ({|"\q\b\f\/\u0041"|}, {|"\q\b\f\/\u0041"|});
      ({|"<div data-post=\"serbia/4\"></div>"|}, {|"<div data-post="serbia/4"></div>"|});
    ]

let method_call_shorthand () = check_desugar "method call shorthand" "(.foo obj 1 2)" "(. obj foo 1 2)"
let explicit_method_call_unchanged () = check_desugar "explicit method call" "(. obj foo 1 2)" "(. obj foo 1 2)"
let constructor_shorthand () = check_desugar "constructor shorthand" "(LocalDate. 2024 1 2)" "(new LocalDate 2024 1 2)"
let let_sequential_pattern () = check_desugar "let sequential pattern" "(let [[a b] xs] a)" "(let* ((list a b) xs) a)"

let keyword_lookup () =
  check_desugar "keyword lookup" "(:TELEGRAM_WEBHOOK_SECRET env)" "(get env \"TELEGRAM_WEBHOOK_SECRET\")"

let keyword_map_key () = check_desugar "keyword map key" "{:key value}" "(hash-map \"key\" value)"

let multiple_import_vectors () =
  check_desugar "multiple import vectors" "(ns app.main (:import [java.time LocalDate] [java.util UUID]))"
    {|(compiler/ns
 "app.main"
 ()
 (("LocalDate" "java.time.LocalDate") ("UUID" "java.util.UUID")))|}

let let_associative_pattern () =
  check_desugar "let associative pattern" "(let [{:name n} user] n)" "(let* ((hash-map \"name\" n) user) n)"

let let_reversed_associative_pattern () =
  check_desugar "let reversed associative pattern" "(let [{url :url props :props} value] (str url props))"
    "(let* ((hash-map \"url\" url \"props\" props) value) (str url props))"

let fn_reversed_associative_pattern () =
  check_desugar "fn reversed associative pattern" "(fn [{url :url}] url)" "(fn* ((hash-map \"url\" url)) url)"

let nested_reversed_associative_pattern () =
  check_desugar "nested reversed associative pattern" "(let [[{name :name} {{city :city} :address}] value] city)"
    {|(let*
 ((list (hash-map "name" name) (hash-map "address" (hash-map "city" city)))
  value)
 city)|}

let reversed_pair_in_map_expression () =
  check_desugar "reversed pair in map expression" "{url :url}" "(hash-map url \"url\")"

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

let private_def () =
  match parse_one "(def- storage 1)" with
  | SList ({ private_; _ }, _, [ SAtom (_, "def"); SAtom (_, "storage"); SAtom (_, "1") ]) ->
      Alcotest.(check bool) "private definition" true private_
  | sexpr -> Alcotest.failf "expected private def, got %s" (Frontend.show_sexpr sexpr)

let private_defn () =
  match parse_one "(defn- helper [x] x)" with
  | SList ({ private_; _ }, _, [ SAtom (_, "def"); SAtom (_, "helper"); SList (_, _, SAtom (_, "fn*") :: _) ]) ->
      Alcotest.(check bool) "private function definition" true private_
  | sexpr -> Alcotest.failf "expected private defn, got %s" (Frontend.show_sexpr sexpr)

let cond_expansion () =
  check_desugar "empty cond" "(cond)" "nil";
  check_desugar "cond pairs" "(cond a x b y)" "(if a x (if b y nil))";
  check_desugar "ordinary else" "(cond :else 1 true 2)" "(if \"else\" 1 (if true 2 nil))"

let cond_metadata () =
  let rec metadata = function
    | SAtom (meta, _) -> [ meta ]
    | SList (meta, _, items) -> meta :: List.concat_map metadata items
  in
  let actual = parse_one "(cond\n  a x\n  b y)" |> metadata in
  let expected =
    List.map
      (fun (line, column) -> { loc = { line; column }; type_annotation = None; private_ = false })
      [ (1, 1); (1, 1); (2, 3); (2, 5); (1, 1); (1, 1); (3, 3); (3, 5); (1, 1) ]
  in
  Alcotest.(check bool) "source and generated metadata" true (actual = expected)

let cond_rejects_incomplete_pairs () =
  List.iter
    (fun input ->
      Alcotest.check_raises input (Failure "cond: expected test/result pairs") (fun () -> ignore (parse_one input)))
    [ "(cond true)"; "(cond true 1 false)"; "(cond :else 1 true)" ]

let case_parts input =
  match parse_one input with
  | SList (_, _, [ SAtom (_, "let*"); SList (_, _, [ SAtom (_, name); value ]); body ]) -> (name, value, body)
  | sexpr -> Alcotest.failf "expected case expansion, got %s" (Frontend.show_sexpr sexpr)

let case_with_fallback () =
  let name, value, body = case_parts "(case (source) 1 one 2 two other)" in
  Alcotest.(check string) "value evaluated in binding" "(source)" (Frontend.show_sexpr value);
  match body with
  | SList
      ( _,
        _,
        [
          SAtom (_, "if");
          SList (_, _, [ SAtom (_, "="); SAtom (_, first_name); SAtom (_, "1") ]);
          SAtom (_, "one");
          SList
            ( _,
              _,
              [
                SAtom (_, "if");
                SList (_, _, [ SAtom (_, "="); SAtom (_, second_name); SAtom (_, "2") ]);
                SAtom (_, "two");
                SAtom (_, "other");
              ] );
        ] ) ->
      Alcotest.(check string) "first comparison uses binding" name first_name;
      Alcotest.(check string) "second comparison uses binding" name second_name
  | sexpr -> Alcotest.failf "expected nested case conditions, got %s" (Frontend.show_sexpr sexpr)

let case_without_fallback () =
  let name, _, body = case_parts "(case value 1 one)" in
  match body with
  | SList
      ( _,
        _,
        [
          SAtom (_, "if");
          SList (_, _, [ SAtom (_, "="); SAtom (_, compared_name); SAtom (_, "1") ]);
          SAtom (_, "one");
          SAtom (_, "nil");
        ] ) ->
      Alcotest.(check string) "comparison uses binding" name compared_name
  | sexpr -> Alcotest.failf "expected nil case fallback, got %s" (Frontend.show_sexpr sexpr)

let if_let_sequential () =
  match parse_one "(if-let [user (first) id (second user)] id fallback)" with
  | SList
      ( _,
        Paren,
        [
          SAtom (_, "let*");
          SList (_, Paren, [ SAtom (_, "user"); SList (_, Paren, [ SAtom (_, "first") ]) ]);
          SList
            ( _,
              Paren,
              [
                SAtom (_, "if");
                SAtom (_, "user");
                SList
                  ( _,
                    Paren,
                    [
                      SAtom (_, "let*");
                      SList (_, Paren, [ SAtom (_, "id"); SList (_, Paren, [ SAtom (_, "second"); SAtom (_, "user") ]) ]);
                      SList (_, Paren, [ SAtom (_, "if"); SAtom (_, "id"); SAtom (_, "id"); SAtom (_, "fallback") ]);
                    ] );
                SAtom (_, "fallback");
              ] );
        ] ) ->
      ()
  | sexpr -> Alcotest.failf "expected sequential if-let expansion, got %s" (Frontend.show_sexpr sexpr)

let if_let_without_else () =
  check_desugar "if-let without else" "(if-let [value (source)] value)" "(let* (value (source)) (if value value nil))"

let check_if_let_failure input message =
  Alcotest.check_raises input (Failure ("if-let: " ^ message)) (fun () -> ignore (parse_one input))

let if_let_rejects_non_symbol_names () =
  List.iter
    (fun input -> check_if_let_failure input "binding names must be symbols")
    [
      "(if-let [[x] value] ok)";
      "(if-let [\"x\" value] ok)";
      "(if-let [1 value] ok)";
      "(if-let [:x value] ok)";
      "(if-let [nil value] ok)";
      "(if-let [true value] ok)";
      "(if-let [false value] ok)";
    ]

let if_let_rejects_malformed_forms () =
  List.iter
    (fun (input, message) -> check_if_let_failure input message)
    [
      ("(if-let [] ok)", "binding vector must not be empty");
      ("(if-let [x] ok)", "bindings must be name/value pairs");
      ("(if-let (x value) ok)", "expected non-empty [name value ...], then branch, and optional else branch");
      ("(if-let [x value])", "expected non-empty [name value ...], then branch, and optional else branch");
      ("(if-let [x value] yes no extra)", "expected non-empty [name value ...], then branch, and optional else branch");
    ]

let () =
  Alcotest.run "frontend desugar"
    [
      ( "interop",
        [
          Alcotest.test_case "string escapes" `Quick string_escapes;
          Alcotest.test_case "method call shorthand" `Quick method_call_shorthand;
          Alcotest.test_case "explicit method call unchanged" `Quick explicit_method_call_unchanged;
          Alcotest.test_case "constructor shorthand" `Quick constructor_shorthand;
          Alcotest.test_case "let sequential pattern" `Quick let_sequential_pattern;
          Alcotest.test_case "keyword lookup" `Quick keyword_lookup;
          Alcotest.test_case "keyword map key" `Quick keyword_map_key;
          Alcotest.test_case "multiple import vectors" `Quick multiple_import_vectors;
          Alcotest.test_case "let associative pattern" `Quick let_associative_pattern;
          Alcotest.test_case "let reversed associative pattern" `Quick let_reversed_associative_pattern;
          Alcotest.test_case "fn reversed associative pattern" `Quick fn_reversed_associative_pattern;
          Alcotest.test_case "nested reversed associative pattern" `Quick nested_reversed_associative_pattern;
          Alcotest.test_case "reversed pair in map expression" `Quick reversed_pair_in_map_expression;
          Alcotest.test_case "annotated fn parameters" `Quick annotated_fn_parameters;
          Alcotest.test_case "annotated defn parameter" `Quick annotated_defn_parameter;
          Alcotest.test_case "private def" `Quick private_def;
          Alcotest.test_case "private defn" `Quick private_defn;
          Alcotest.test_case "case with fallback" `Quick case_with_fallback;
          Alcotest.test_case "cond expansion" `Quick cond_expansion;
          Alcotest.test_case "cond metadata" `Quick cond_metadata;
          Alcotest.test_case "cond rejects incomplete pairs" `Quick cond_rejects_incomplete_pairs;
          Alcotest.test_case "case without fallback" `Quick case_without_fallback;
          Alcotest.test_case "sequential if-let" `Quick if_let_sequential;
          Alcotest.test_case "if-let without else" `Quick if_let_without_else;
          Alcotest.test_case "if-let rejects non-symbol names" `Quick if_let_rejects_non_symbol_names;
          Alcotest.test_case "if-let rejects malformed forms" `Quick if_let_rejects_malformed_forms;
        ] );
    ]
