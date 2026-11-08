open Frontend

let lower ?(java = false) input =
  match Frontend.parse_and_desugar input with
  | Ok sexprs -> Gensym.run (fun () -> Backend_compiler.Lowering_expression_to_statement.lower ~java sexprs)
  | Error message -> Alcotest.fail message

let rec has_symbol_only_function_bindings = function
  | SAtom _ -> true
  | SList (_, _, SAtom (_, "fn*") :: SList (_, _, params) :: body) ->
      List.for_all (function SAtom _ -> true | _ -> false) params
      && List.for_all has_symbol_only_function_bindings body
  | SList (_, _, SAtom (_, "let*") :: SAtom _ :: value :: body) ->
      has_symbol_only_function_bindings value && List.for_all has_symbol_only_function_bindings body
  | SList (_, _, items) -> List.for_all has_symbol_only_function_bindings items

let function_patterns_are_normalized () =
  Alcotest.(check bool)
    "symbol-only function parameters and bindings" true
    (lower {|(def f (fn [[a {:name n}]] (str a n)))|} |> List.for_all has_symbol_only_function_bindings)

let instance_check () =
  let input = "(instance? (do String) value)" in
  let parsed = match Frontend.parse_and_desugar input with Ok forms -> forms | Error e -> Alcotest.fail e in
  Alcotest.(check bool) "preserves type expression and metadata" true (parsed = lower input);
  match lower "(do (instance? Object (effect)) nil)" with
  | [
   SList
     ( _,
       _,
       [
         SAtom (_, "do");
         SList
           ( _,
             _,
             [
               SAtom (_, "let*");
               SAtom (_, _);
               SList (_, _, [ SAtom (_, "instance?"); SAtom (_, "Object"); SList (_, _, [ SAtom (_, "effect") ]) ]);
             ] );
         SAtom (_, "nil");
       ] );
  ] ->
      ()
  | _ -> Alcotest.fail "discarded instance? must bind its value once"

let raw_code_preservation () =
  let input = {|(defn test [] (raw-code "a();") (raw-code "b();") nil)|} in
  let parsed = match Frontend.parse_and_desugar input with Ok forms -> forms | Error e -> Alcotest.fail e in
  Alcotest.(check bool) "no generated bindings for raw instructions" true (lower input = parsed);
  List.iter
    (fun source -> ignore (lower source))
    [
      {|(raw-code "")|};
      {|(quote (raw-code :invalid))|};
      {|(defn test [] (let [] (raw-code "x") nil))|};
      {|(defn test [] (do (raw-code "x") nil))|};
      {|(defn test [] (if true (do (raw-code "x") nil) nil))|};
      {|(defn test [] (f (fn [] (raw-code "x") nil)))|};
    ]

let operand_order () =
  (* Moving the late do prefix ahead of an earlier value changes effects and host reads. *)
  let rec events = function
    | SAtom _ -> []
    | SList (_, _, [ SAtom (_, "quote"); _ ]) -> []
    | SList (_, _, SAtom (_, "let*") :: SAtom _ :: value :: rest) -> events value @ List.concat_map events rest
    | SList (_, _, SAtom (_, name) :: args) ->
        List.concat_map events args @ if List.mem name [ "early"; "late"; "callee" ] then [ name ] else []
    | SList (_, _, items) -> List.concat_map events items
  in
  List.iter
    (fun source -> Alcotest.(check (list string)) source [ "early"; "late" ] (lower source |> List.concat_map events))
    [
      "(f (early) (do (late) 2))";
      "(. (early) method (do (late) 2))";
      "(new Host (early) (do (late) 2))";
      "[(early) (do (late) 2)]";
      "{:a (early) :b (do (late) 2)}";
      "(cast Host (f (early) (do (late) 2)))";
      "(instance? Host (f (early) (do (late) 2)))";
      "(f (early) (try (late) (finally nil)))";
      "(. (early) method (try (late) (finally nil)))";
      "[(early) (try (late) (finally nil))]";
      "{:a (early) :b (try (late) (finally nil))}";
      "(f (try (early) (finally nil)) (try (late) (finally nil)))";
    ];
  Alcotest.(check (list string))
    "dynamic callee precedes try" [ "callee"; "early"; "late" ]
    (lower "((callee) (early) (try (late) (finally nil)))" |> List.concat_map events);
  Alcotest.(check (list string))
    "dynamic callee evaluated once before argument prefixes" [ "callee"; "early"; "late" ]
    (lower "((callee) (early) (do (late) 2))" |> List.concat_map events);
  match lower "(. ^String receiver concat (try (late) (finally nil)))" with
  | [
   SList (_, _, SAtom (_, "do") :: SList (_, _, [ SAtom (_, "let*"); SAtom (meta, _); SAtom (_, "receiver") ]) :: _);
  ] ->
      Alcotest.(check (option string)) "receiver type retained" (Some "String") meta.type_annotation
  | _ -> Alcotest.fail "receiver read must precede the later prefix"

let statement_if_generation () =
  let contains text part =
    let n = String.length part in
    List.exists (fun i -> String.sub text i n = part) (List.init (max 0 (String.length text - n + 1)) Fun.id)
  in
  List.iter
    (fun source ->
      let forms = match Frontend.parse_and_desugar source with Ok x -> x | Error e -> Alcotest.fail e in
      List.iter
        (fun (target, code, wrapper) ->
          Alcotest.(check bool) (target ^ " native if: " ^ source) true (contains code "if (");
          Alcotest.(check bool) (target ^ " no switch: " ^ source) false (contains code "switch (");
          Alcotest.(check bool) (target ^ " no wrapper: " ^ source) false (contains code wrapper))
        [ ("JS", Backend_compiler.Js.compile forms, "})()"); ("Java", Backend_compiler.Java.compile forms, "Supplier") ])
    [
      "(defn f [] (if true 1 2))";
      "(defn f [] (str (if true 1 2)))";
      "(defn f [] (if (if true false true) 1 2))";
      "(defn f [] (let [x (if true 1 2)] x))";
      "(defn f [] (do (let [x 1] (if true (do (let [y x] y)) 2))))";
      "(defn f [] (if true (println 1) (println 2)) nil)";
      "(def x (if true 1 2))";
      "(def x (str (let [a (if true 1 2)] a)))";
      {|(defn f [] (let [^String text (let [x (new String "a")] x)] (.concat text (str (if true "b" "unused")))))|};
      {|(defn f [] (. (cast String (let [x (String/valueOf "a")] x)) concat (str (if true "b" "unused"))))|};
    ]

let exception_grammar () =
  List.iter
    (fun source -> ignore (lower source))
    [
      "(try 1 (catch e 2))";
      "(try 1 (finally nil))";
      "(try 1 (catch e 2) (finally nil))";
      "(throw e)";
      "'(try)";
      "'(throw)";
    ];
  List.iter
    (fun source ->
      match lower source with
      | _ -> Alcotest.fail ("accepted invalid exception form: " ^ source)
      | exception Failure message ->
          Alcotest.(check bool)
            "diagnostic names form" true
            (String.starts_with ~prefix:(if String.starts_with ~prefix:"(throw" source then "throw" else "try") message))
    [
      "(try)";
      "(try 1)";
      "(try (catch e 2))";
      "(try 1 (catch e))";
      "(try 1 (finally))";
      "(try 1 (catch e 2) (catch e 3))";
      "(try 1 (finally nil) (finally nil))";
      "(try 1 (finally nil) (catch e 2))";
      "(try 1 (catch e 2) 3)";
      "(throw)";
      "(throw e extra)";
    ];
  List.iter
    (fun binding ->
      let source = "(try 1 (catch " ^ binding ^ " 2))" in
      match lower source with _ -> Alcotest.fail ("accepted catch binding: " ^ binding) | exception Failure _ -> ())
    [ "nil"; "false"; "true"; "1"; "\"error\""; ":e"; "ns/e"; "[e]"; "{:e e}" ]

let exception_regions () =
  let rec assignments = function
    | SAtom _ -> []
    | SList (_, _, [ SAtom (_, "set!"); SAtom (_, name); _ ]) -> [ name ]
    | SList (_, _, items) -> List.concat_map assignments items
  in
  (match lower "(try (work) (catch e (recover e)) (finally (clean)))" with
  | [
   SList
     ( _,
       _,
       [
         SAtom (_, "do");
         SList (_, _, [ SAtom (_, "let*"); SAtom (_, result); SAtom (_, "nil") ]);
         SList (_, _, SAtom (_, "try") :: body :: sections);
         SAtom (_, value);
       ] );
  ] ->
      Alcotest.(check string) "returns result" result value;
      Alcotest.(check (list string)) "body writes result" [ result ] (assignments body);
      Alcotest.(check (list string))
        "only handler writes result, cleanup does not" [ result ] (List.concat_map assignments sections)
  | _ -> Alcotest.fail "value try must contain only initialization, protected statement, and result");
  match lower "(do (try (work) (catch e (recover e)) (finally (clean))) nil)" with
  | [ SList (_, _, [ SAtom (_, "do"); (SList (_, _, SAtom (_, "try") :: _) as protected); SAtom (_, "nil") ]) ] ->
      Alcotest.(check (list string)) "discard try has no result assignments" [] (assignments protected)
  | _ -> Alcotest.fail "discard try must not allocate a result"

let exception_scope () =
  (match lower ~java:true "(do (try (work) (catch e e)) nil)" with
  | [
   SList
     ( _,
       _,
       [
         SAtom (_, "do");
         SList (_, _, [ SAtom (_, "try"); _; SList (_, _, [ SAtom (_, "catch"); SAtom (_, name); _ ]) ]);
         _;
       ] );
  ] ->
      Alcotest.(check bool) "Java catch uses a fresh host name" false (name = "e")
  | _ -> Alcotest.fail "missing catch binding");
  let rec check = function
    | SList
        (_, _, [ SAtom (_, "catch"); SAtom (_, caught); SList (_, _, SAtom (_, "let*") :: SList (_, _, []) :: body) ])
      ->
        (match List.rev body with
        | SList (_, _, [ SAtom (_, "throw"); SAtom (_, name) ]) :: _ ->
            Alcotest.(check string) "rethrow keeps catch binding after nested lets" caught name
        | _ -> Alcotest.fail "handler must finish with rethrow");
        let blocks =
          List.filter (function SList (_, _, SAtom (_, "let*") :: SList (_, _, []) :: _) -> true | _ -> false) body
        in
        Alcotest.(check int) "each discarded let keeps its own scope" 3 (List.length blocks)
    | SList (_, _, items) -> List.iter check items
    | SAtom _ -> ()
  in
  List.iter
    (fun java ->
      lower ~java {|(do (try (work) (catch e (let [e "inner"] e) (let [x 1] x) (let [x 2] x) (throw e))) nil)|}
      |> List.iter check)
    [ false; true ]

let exception_branch () =
  let rec count = function
    | SAtom _ -> 0
    | SList (_, _, SAtom (_, "try") :: _) -> 1
    | SList (_, _, items) -> List.fold_left (fun n x -> n + count x) 0 items
  in
  match lower "(if false (try (work) (finally (clean))) nil)" with
  | [ SList (_, _, [ SAtom (_, "do"); _; SList (_, _, [ SAtom (_, "if"); _; then_; else_ ]); _ ]) ] ->
      Alcotest.(check int) "try stays in selected branch" 1 (count then_);
      Alcotest.(check int) "other branch has no protected effects" 0 (count else_)
  | _ -> Alcotest.fail "try must remain within native if"

let exception_generation java () =
  let contains text part =
    let n = String.length part in
    List.exists (fun i -> String.sub text i n = part) (List.init (max 0 (String.length text - n + 1)) Fun.id)
  in
  List.iter
    (fun source ->
      let forms = match Frontend.parse_and_desugar source with Ok x -> x | Error e -> Alcotest.fail e in
      let code = if java then Backend_compiler.Java.compile forms else Backend_compiler.Js.compile forms in
      Alcotest.(check bool) ("native try: " ^ source) true (contains code "try {");
      Alcotest.(check bool) ("no wrapper: " ^ source) false (contains code (if java then "Supplier" else "})()")))
    [
      "(defn f [] (try 1 (finally nil)))";
      "(defn f [] (str (try 1 (catch e 2))))";
      "(defn f [] (if (try false (finally nil)) 1 2))";
      "(defn f [] (let [x (try 1 (finally nil))] x))";
      "(defn f [] (try 1 (finally nil)) nil)";
      "(defn f [] (if true (do (let [x 1] (try x (finally nil)))) 2))";
      "(def x (try 1 (finally nil)))";
      "(def x (str (let [x (if true (try 1 (finally nil)) 2)] x)))";
    ]

let throw_try_generation () =
  let count text part =
    let n = String.length part in
    List.init (max 0 (String.length text - n + 1)) Fun.id
    |> List.filter (fun i -> String.sub text i n = part)
    |> List.length
  in
  List.iter
    (fun (source, arrows) ->
      let forms = match Frontend.parse_and_desugar source with Ok x -> x | Error e -> Alcotest.fail e in
      let js = Backend_compiler.Js.compile forms in
      Alcotest.(check int) "one native JS try" 1 (count js "try {");
      Alcotest.(check int) "only actual function and optional value-throw wrapper" arrows (count js "=>");
      let java = Backend_compiler.Java.compile forms in
      Alcotest.(check int) "one native Java try" 1 (count java "try {");
      Alcotest.(check int) "throw helper called once" 1 (count java "throw_value(");
      Alcotest.(check int) "no Java lambda around try or throw" 0 (count java "->");
      Alcotest.(check int) "no Java Supplier" 0 (count java "Supplier"))
    [ ("(defn f [e] (throw (try e (finally nil))))", 2); ("(defn f [e] (throw (try e (finally nil))) nil)", 1) ]

let () =
  Alcotest.run "lowering"
    [
      ("fn patterns", [ Alcotest.test_case "normalizes destructuring" `Quick function_patterns_are_normalized ]);
      ("instance?", [ Alcotest.test_case "type preservation and discard" `Quick instance_check ]);
      ("raw-code", [ Alcotest.test_case "statement preservation" `Quick raw_code_preservation ]);
      ("operands", [ Alcotest.test_case "prefix order and single evaluation" `Quick operand_order ]);
      ("if", [ Alcotest.test_case "native statements in value positions" `Quick statement_if_generation ]);
      ( "exceptions",
        [
          Alcotest.test_case "grammar and quote" `Quick exception_grammar;
          Alcotest.test_case "protected results and discard" `Quick exception_regions;
          Alcotest.test_case "handler lexical blocks" `Quick exception_scope;
          Alcotest.test_case "branch boundary" `Quick exception_branch;
          Alcotest.test_case "native JS generation" `Quick (exception_generation false);
          Alcotest.test_case "native Java generation" `Quick (exception_generation true);
          Alcotest.test_case "try in throw stays outside wrappers" `Quick throw_try_generation;
        ] );
    ]
