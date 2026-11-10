open Frontend
open Backend_eval.Eval_types
module Eval = Backend_eval.Eval
module Macros = Backend_eval.Language_macros

let call_meta = { loc = { line = 2; column = 3 }; type_annotation = None; private_ = false }
let source_meta = { loc = { line = 5; column = 7 }; type_annotation = Some "Runnable"; private_ = true }

let mixed_tree_preserves_metadata () =
  let source = SList (source_meta, Bracket, [ SAtom (source_meta, "x") ]) in
  let result = Macros.to_sexpr call_meta (List [ Symbol "if"; Bool true; Syntax source; Nil ]) in
  let expected =
    SList (call_meta, Paren, [ SAtom (call_meta, "if"); SAtom (call_meta, "true"); source; SAtom (call_meta, "nil") ])
  in
  Alcotest.(check bool) "new nodes get call metadata" true (result = expected);
  match result with
  | SList (_, _, [ _; _; original; _ ]) -> Alcotest.(check bool) "source node reused intact" true (original == source)
  | _ -> Alcotest.fail "expected mixed tree"

let scalar_categories () =
  List.iter
    (fun value ->
      let form = Macros.to_sexpr call_meta value in
      Alcotest.(check bool) "literal value and constructor preserved" true (Eval.eval form = value))
    [ Nil; Bool false; Bool true; Int 1; Float 1.; Float 0.125; String "nil"; String "false"; String "a\"b\\c\nd" ];
  Alcotest.(check bool) "symbol stays a symbol" true (Eval.quote (Macros.to_sexpr call_meta (Symbol "if")) = Symbol "if")

let parse_one source =
  match Frontend.parse_string source with Ok [ form ] -> form | _ -> Alcotest.fail "expected one parsed form"

let language_cond () =
  let macros = Macros.create () in
  List.iter
    (fun (source, expected) ->
      match List.find_map (fun macro -> macro (parse_one source)) macros with
      | Some form -> Alcotest.(check string) source expected (Frontend.show_sexpr form)
      | None -> Alcotest.fail "cond was not recognized")
    [ ("(cond)", "nil"); ("(cond\n false 1\n true 42)", "(if false 1 (if true 42 nil))") ];
  List.iter
    (fun source ->
      Alcotest.check_raises source (Failure "cond: expected test/result pairs") (fun () ->
          ignore (List.find_map (fun macro -> macro (parse_one source)) macros)))
    [ "(cond true)"; "(cond true 1 false)"; "(cond :else 1 true)" ]

let cond_definition_diagnostics () =
  let _, entry, source = List.find (fun (name, _, _) -> name = "cond") Backend_eval.Builtin_macro_source.definitions in
  let macros = Macros.create_from [ ("choose", entry, source) ] () in
  let expand source = Option.get (List.find_map (fun macro -> macro (parse_one source)) macros) in
  List.iter (fun (source, expected) ->
    Alcotest.(check bool) source true (Eval.eval (expand source) = expected))
    [ "(choose)", Nil; "(choose false 1 true 42)", Int 42; "(choose true false)", Bool false ];
  List.iter (fun source ->
    Alcotest.check_raises source (Failure "cond: expected test/result pairs") (fun () ->
      ignore (expand source)))
    [ "(choose true)"; "(choose true 42 (reset! must-not-run 1))" ]

let custom_handler () =
  let source = {|(def expand-probe (fn [args]
    (let [v (get args 0)]
      (if (syntax-vector? v)
        (if (syntax-simple-symbol? (get (syntax-children v) 0))
          (list (gensym) (syntax-list-at v (syntax-children v)))
          (macro-error "expected symbol"))
        v))))|} in
  let macros = Macros.create_from [ ("probe", "expand-probe", source) ] () in
  fun form -> List.find_map (fun macro -> macro form) macros

let custom_definition () =
  let handle = custom_handler () in
  Alcotest.(check bool) "unknown form falls through" true (handle (parse_one "(unknown 1)") = None);
  let child = SAtom (source_meta, "x") in
  let vector = SList (source_meta, Bracket, [ child ]) in
  Gensym.run (fun () ->
    match handle (SList (call_meta, Paren, [ SAtom (call_meta, "probe"); vector ])) with
    | Some (SList (meta, Paren, [ SAtom (name_meta, _); SList (vector_meta, Paren, [ original ]) ])) ->
        Alcotest.(check bool) "current call and donor metadata" true
          (meta = call_meta && name_meta = call_meta && vector_meta = source_meta);
        Alcotest.(check bool) "source child retained" true (original == child)
    | _ -> Alcotest.fail "expected generated name and syntax list")

let custom_call_context () =
  let handle = custom_handler () in
  let call meta arg = SList (meta, Paren, [ SAtom (meta, "probe"); arg ]) in
  let original = parse_one "(reset! must-not-run 1)" in
  (* No Gensym.run: any accidental allocation raises an unhandled effect. *)
  Alcotest.(check bool) "identity neither evaluates nor generates" true
    (Option.get (handle (call call_meta original)) == original);
  Gensym.run (fun () ->
    let generate meta =
      match handle (call meta (parse_one "[x]")) with
      | Some (SList (m, Paren, [ SAtom (nm, name); _ ])) ->
          Alcotest.(check bool) "metadata refreshed" true (m = meta && nm = meta);
          name
      | _ -> Alcotest.fail "expected generated name" in
    let first = generate call_meta in
    Alcotest.check_raises "macro error" (Failure "expected symbol") (fun () ->
      ignore (handle (call call_meta (parse_one "[42]"))));
    let second = generate source_meta in
    Alcotest.(check int) "common fresh-name sequence" 3
      (List.length (List.sort_uniq String.compare [ first; second; Gensym.gensym_string call_meta ])))

let definition_state_isolation () =
  let definitions = [ ("tick", "expand-tick", {|(def counter (atom 0))
    (def expand-tick (fn [args] (swap! counter (fn [n] (+ n 1)))))|}) ] in
  let instance () =
    let macros = Macros.create_from definitions () in
    fun () -> Eval.eval (Option.get (List.find_map (fun macro -> macro (parse_one "(tick)")) macros)) in
  let first = instance () in
  let second = instance () in
  Alcotest.(check bool) "instances own their state" true
    (first () = Int 1 && first () = Int 2 && second () = Int 1 && first () = Int 3)

let language_and () =
  Gensym.run (fun () ->
      let macros = Macros.create () in
      let expand source =
        match List.find_map (fun macro -> macro source) macros with
        | Some form -> form
        | None -> Alcotest.fail "and was not recognized"
      in
      Alcotest.(check bool) "empty and is boolean true" true (Eval.eval (expand (parse_one "(and)")) = Bool true);
      let value = SList (source_meta, Paren, [ SAtom (source_meta, "effect") ]) in
      let call items = SList (call_meta, Paren, SAtom (call_meta, "and") :: items) in
      Alcotest.(check bool) "single argument reused" true (expand (call [ value ]) == value);
      let last = SAtom (source_meta, "42") in
      match expand (call [ value; last ]) with
      | SList (meta, Paren,
          [ SAtom (let_meta, "let*"); SList (bindings_meta, Paren, [ SAtom (name_meta, name); original ]);
            SList (if_meta, Paren, [ SAtom (head_meta, "if"); SAtom (test_meta, test); result; SAtom (else_meta, otherwise) ]) ]) ->
          Alcotest.(check bool) "generated nodes use call metadata" true
            (List.for_all (( = ) call_meta) [ meta; let_meta; bindings_meta; name_meta; if_meta; head_meta; test_meta; else_meta ]);
          Alcotest.(check bool) "arguments retain nodes and annotations" true (original == value && result == last);
          Alcotest.(check bool) "condition and false branch reference binding" true (test = name && otherwise = name);
          Alcotest.(check bool) "uses shared gensym" true (name <> Gensym.gensym_string call_meta)
      | _ -> Alcotest.fail "expected let*/if expansion")

let language_or () =
  Gensym.run (fun () ->
      let macros = Macros.create () in
      let expand source =
        match List.find_map (fun macro -> macro source) macros with
        | Some form -> form
        | None -> Alcotest.fail "or was not recognized"
      in
      Alcotest.(check bool) "empty or is Nil" true (Eval.eval (expand (parse_one "(or)")) = Nil);
      let value = SList (source_meta, Paren, [ SAtom (source_meta, "effect") ]) in
      let last = SAtom (source_meta, "42") in
      List.iter
        (fun expected_meta ->
          let call items = SList (expected_meta, Paren, SAtom (expected_meta, "or") :: items) in
          Alcotest.(check bool) "single argument reused" true (expand (call [ value ]) == value);
          match expand (call [ value; last ]) with
          | SList (meta, Paren,
              [ SAtom (let_meta, "let*"); SList (bindings_meta, Paren, [ SAtom (name_meta, name); original ]);
                SList (if_meta, Paren, [ SAtom (head_meta, "if"); SAtom (test_meta, test); SAtom (then_meta, selected); result ]) ]) ->
              Alcotest.(check bool) "new nodes use current call metadata" true
                (List.for_all (( = ) expected_meta)
                   [ meta; let_meta; bindings_meta; name_meta; if_meta; head_meta; test_meta; then_meta ]);
              Alcotest.(check bool) "arguments retain nodes and annotations" true (original == value && result == last);
              Alcotest.(check bool) "condition and true branch reference binding" true (test = name && selected = name)
          | _ -> Alcotest.fail "expected let*/if expansion")
        [ call_meta; { call_meta with loc = { line = 10; column = 8 } } ])

let with_open_handler () =
  let macros = Macros.create () in
  fun source ->
    match List.find_map (fun macro -> macro source) macros with
    | Some form -> form
    | None -> Alcotest.fail "with-open was not recognized"

let language_with_open () =
  let expand = with_open_handler () in
  List.iter
    (fun (source, expected) ->
      Alcotest.(check string) source (Frontend.show_sexpr (parse_one expected))
        (Frontend.show_sexpr (expand (parse_one source))))
    [
      ("(with-open [])", "nil");
      ("(with-open [] 1 false)", "(do 1 false)");
      ("(with-open [r (source)])", "(let (r (source)) (try nil (finally (.close (cast java.lang.AutoCloseable r)))))");
      ("(with-open [a (open-a) b (open-b a)] (use a b))",
       "(let (a (open-a)) (try (let (b (open-b a)) (try (do (use a b)) (finally (.close (cast java.lang.AutoCloseable b))))) (finally (.close (cast java.lang.AutoCloseable a)))))");
    ];
  let vector_meta = { source_meta with loc = { line = 3; column = 4 }; type_annotation = None } in
  let name = SAtom (source_meta, "r") in
  let initial_value = parse_one "(reset! must-not-run 1)" in
  let body = parse_one "(effect r)" in
  let vector = SList (vector_meta, Bracket, [ name; initial_value ]) in
  List.iter
    (fun meta ->
      let result = expand (SList (meta, Paren, [ SAtom (meta, "with-open"); vector; body ])) in
      let rec check = function
        | node when node == name || node == initial_value || node == body -> ()
        | SList (m, Paren, [ n; value ]) when n == name && value == initial_value ->
            Alcotest.(check bool) "binding container inherits vector metadata" true (m = vector_meta)
        | SList (m, Paren, items) ->
            Alcotest.(check bool) "generated list metadata" true (m = meta);
            List.iter check items
        | SAtom (m, _) -> Alcotest.(check bool) "generated atom metadata" true (m = meta)
        | _ -> Alcotest.fail "unexpected generated bracket"
      in
      check result;
      match result with
      | SList (_, _, [ _; SList (_, _, [ n; value ]); SList (_, _, [ _; SList (_, _, [ _; b ]); _ ]) ]) ->
          Alcotest.(check bool) "source children reused intact" true (n == name && value == initial_value && b == body)
      | _ -> Alcotest.fail "expected let/try/do")
    [ call_meta; { call_meta with loc = { line = 12; column = 9 } } ];
  Alcotest.(check bool) "input vector unchanged" true (vector = SList (vector_meta, Bracket, [ name; initial_value ]))

let with_open_diagnostics () =
  let expand = with_open_handler () in
  List.iter
    (fun source ->
      Alcotest.check_raises source (Failure "with-open: expected a binding vector") (fun () ->
          ignore (expand (parse_one source))))
    [ "(with-open)"; "(with-open (r value) body)"; "(with-open nil)"; "(with-open {})" ];
  List.iter
    (fun bindings ->
      let source = "(with-open [" ^ bindings ^ "] body)" in
      Alcotest.check_raises source (Failure "with-open: expected simple symbol/value pairs") (fun () ->
          ignore (expand (parse_one source))))
    [ "r"; "r value s"; "[r] value"; "{} value"; "\"r\" value"; "1 value"; ":r value";
      "nil value"; "true value"; "false value"; "ns/r value" ]

let with_open_runtime_isolation () =
  let names =
    [ "expand-with-open"; "expand-with-open-bindings"; "syntax-vector?"; "syntax-children";
      "syntax-simple-symbol?"; "syntax-list-at"; "macro-error" ]
  in
  let definitions = String.concat " " (List.map (fun name -> "(def " ^ name ^ " 99)") names) in
  let source = definitions ^ " (defn task [] (with-open [r (source)] 42)) (with-open [] 7)" in
  let eval context source =
    match Backend_eval.Pipeline.parse_and_desugar source with
    | Ok forms -> Eval.eval_all ~context forms
    | Error message -> Alcotest.fail message
  in
  let context = Backend_eval.Pipeline.create_context () in
  Alcotest.(check bool) "bootstrap leaves runtime empty" true (!(context.namespaces) = []);
  let results = eval context source in
  Alcotest.(check bool) "user helpers cannot replace compile-time helpers" true (List.hd (List.rev results) = Int 7);
  List.iter (fun name -> Alcotest.(check bool) name true (Eval.find context name = Int 99)) names;
  (match !(context.namespaces) with
  | [ ("user", bindings) ] ->
      Alcotest.(check (list string)) "only user definitions" (List.sort String.compare ("task" :: names))
        (List.sort String.compare (List.map fst bindings))
  | _ -> Alcotest.fail "expected only user namespace");
  Alcotest.check_raises "user macro-error cannot suppress diagnostics" (Failure "with-open: expected a binding vector")
    (fun () -> ignore (eval context (definitions ^ " (with-open)")));
  let other = Backend_eval.Pipeline.create_context () in
  Alcotest.(check bool) "independent file" true (eval other "(with-open [] 42)" = [ Int 42 ]);
  Alcotest.(check bool) "previous definitions and helpers absent" true (!(other.namespaces) = [])

let boolean_macro_runtime_isolation macro () =
  let context = Backend_eval.Pipeline.create_context () in
  let internal_name = "expand-" ^ macro in
  let condition = if macro = "and" then "true" else "false" in
  let source = Printf.sprintf "(def %s 99) (def gensym 7) (%s %s 42)" internal_name macro condition in
  let forms =
    match Backend_eval.Pipeline.parse_and_desugar source with
    | Ok forms -> forms
    | Error message -> Alcotest.fail message
  in
  Alcotest.(check bool) "user names cannot replace internal definitions" true
    (Eval.eval_all ~context forms = [ Int 99; Int 7; Int 42 ]);
  Alcotest.(check bool) "only user names in runtime" true
    (!(context.namespaces) = [ ("user", [ ("gensym", Int 7); (internal_name, Int 99) ]) ]);
  let other = Backend_eval.Pipeline.create_context () in
  ignore (Eval.eval_all ~context:other forms);
  Alcotest.(check bool) "new runtime starts without internal helpers" true
    (!(other.namespaces) = !(context.namespaces))

let shared_gensym () =
  let source = "(and (or false (and true 3)) (case (and true 7) 7 (and true 42) 0)) (and true 9) (or (or nil false) (and true 11))" in
  let forms =
    match Backend_eval.Pipeline.parse_and_desugar source with
    | Ok forms -> forms
    | Error message -> Alcotest.fail message
  in
  let rec bindings = function
    | SList (_, Paren, [ SAtom (_, "let*"); SList (_, Paren, [ SAtom (_, name); value ]); body ]) ->
        name :: (bindings value @ bindings body)
    | SList (_, _, items) -> List.concat_map bindings items
    | SAtom _ -> []
  in
  let names = List.concat_map bindings forms in
  Alcotest.(check int) "all generated bindings visited" 10 (List.length names);
  Alcotest.(check int) "fresh across language and OCaml macros" 10 (List.length (List.sort_uniq String.compare names));
  Alcotest.(check bool) "references resolve to their own binding" true (Eval.eval_all forms = [ Int 42; Int 9; Int 11 ])

let runtime_isolation () =
  let context = Backend_eval.Pipeline.create_context () in
  let eval source =
    match Backend_eval.Pipeline.parse_and_desugar source with
    | Ok forms -> Eval.eval_all ~context forms
    | Error message -> Alcotest.fail message
  in
  Alcotest.(check bool) "bootstrap leaves runtime empty" true (!(context.namespaces) = []);
  let result = eval "(def expand-cond 99) (cond false 1 true expand-cond)" in
  Alcotest.(check bool) "user binding cannot replace macro" true (result = [ Int 99; Int 99 ]);
  Alcotest.(check bool)
    "only user definitions in runtime" true
    (!(context.namespaces) = [ ("user", [ ("expand-cond", Int 99) ]) ]);
  let other_context = Backend_eval.Pipeline.create_context () in
  let forms =
    match Backend_eval.Pipeline.parse_and_desugar "(cond true 42)" with
    | Ok forms -> forms
    | Error message -> Alcotest.fail message
  in
  Alcotest.(check bool)
    "independent source expands normally" true
    (Eval.eval_all ~context:other_context forms = [ Int 42 ]);
  Alcotest.(check bool) "internal and previous user names absent" true (!(other_context.namespaces) = [])

let () =
  Alcotest.run "language macros"
    [
      ( "bridge",
        [
          Alcotest.test_case "mixed tree metadata" `Quick mixed_tree_preserves_metadata;
          Alcotest.test_case "scalar categories" `Quick scalar_categories;
        ] );
      ( "definitions",
        [ Alcotest.test_case "arbitrary name and helpers" `Quick custom_definition;
          Alcotest.test_case "call context" `Quick custom_call_context;
          Alcotest.test_case "state isolation" `Quick definition_state_isolation ] );
      ( "or", [ Alcotest.test_case "language definition and metadata" `Quick language_or;
                 Alcotest.test_case "runtime isolation" `Quick (boolean_macro_runtime_isolation "or") ] );
      ( "with-open",
        [ Alcotest.test_case "structure and source metadata" `Quick language_with_open;
          Alcotest.test_case "form diagnostics" `Quick with_open_diagnostics;
          Alcotest.test_case "runtime isolation" `Quick with_open_runtime_isolation ] );
      ( "and",
        [ Alcotest.test_case "language definition and metadata" `Quick language_and;
          Alcotest.test_case "shared gensym" `Quick shared_gensym;
          Alcotest.test_case "runtime isolation" `Quick (boolean_macro_runtime_isolation "and") ] );
      ( "cond",
        [
          Alcotest.test_case "language definition" `Quick language_cond;
          Alcotest.test_case "definition diagnostics" `Quick cond_definition_diagnostics;
          Alcotest.test_case "runtime isolation" `Quick runtime_isolation;
        ] );
    ]
