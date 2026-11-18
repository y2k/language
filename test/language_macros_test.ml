open Frontend
open Backend_eval.Eval_types
module Eval = Backend_eval.Eval
module Language_macros = Macros.Language_macros

let call_meta = { loc = { line = 2; column = 3 }; type_annotation = None; private_ = false }
let source_meta = { loc = { line = 5; column = 7 }; type_annotation = Some "Runnable"; private_ = true }

let mixed_tree_preserves_metadata () =
  let source = SList (source_meta, Bracket, [ SAtom (source_meta, "x") ]) in
  let result = Language_macros.to_sexpr call_meta (List [ Symbol "if"; Bool true; Syntax source; Nil ]) in
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
      let form = Language_macros.to_sexpr call_meta value in
      Alcotest.(check bool) "literal value and constructor preserved" true (Eval.eval form = value))
    [ Nil; Bool false; Bool true; Int 1; Float 1.; Float 0.125; String "nil"; String "false"; String "a\"b\\c\nd" ];
  Alcotest.(check bool) "symbol stays a symbol" true (Eval.quote (Language_macros.to_sexpr call_meta (Symbol "if")) = Symbol "if")

let parse_one source =
  match Frontend.parse_string source with Ok [ form ] -> form | _ -> Alcotest.fail "expected one parsed form"

let language_cond () =
  let macros = Language_macros.create () in
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
  let source = Language_macros.read_prelude () ^ "\n(def choose cond)" in
  let macros = Language_macros.create_from [ "choose" ] source () in
  let expand source = Option.get (List.find_map (fun macro -> macro (parse_one source)) macros) in
  List.iter (fun (source, expected) ->
    Alcotest.(check bool) source true (Eval.eval (expand source) = expected))
    [ "(choose)", Nil; "(choose false 1 true 42)", Int 42; "(choose true false)", Bool false ];
  List.iter (fun source ->
    Alcotest.check_raises source (Failure "cond: expected test/result pairs") (fun () ->
      ignore (expand source)))
    [ "(choose true)"; "(choose true 42 (reset! must-not-run 1))" ]

let custom_handler () =
  let source = {|(def probe (fn [args]
    (let [v (get args 0)]
      (if (syntax-vector? v)
        (if (syntax-simple-symbol? (get (syntax-children v) 0))
          (list (gensym) (syntax-list-at v (syntax-children v)))
          (macro-error "expected symbol"))
        v))))|} in
  let macros = Language_macros.create_from [ "probe" ] source () in
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
  let source = {|(def counter (atom 0))
    (def tick (fn [args] (swap! counter (fn [n] (+ n 1)))))|} in
  let instance () =
    let macros = Language_macros.create_from [ "tick" ] source () in
    fun () -> Eval.eval (Option.get (List.find_map (fun macro -> macro (parse_one "(tick)")) macros)) in
  let first = instance () in
  let second = instance () in
  Alcotest.(check bool) "instances own their state" true
    (first () = Int 1 && first () = Int 2 && second () = Int 1 && first () = Int 3)

let recursion_by_form_name () =
  let source = {|(def cond (fn [clauses] (if (= clauses (list)) 0 (+ 1 (cond (drop 1 clauses))))))|} in
  let macros = Language_macros.create_from [ "cond" ] source () in
  Alcotest.(check bool) "recursive call stays a compile-time function call" true
    (Eval.eval (Option.get (List.find_map (fun macro -> macro (parse_one "(cond a b c)")) macros)) = Int 3)

let language_and () =
  Gensym.run (fun () ->
      let macros = Language_macros.create () in
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
      let macros = Language_macros.create () in
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
  let macros = Language_macros.create () in
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

let language_if_let_metadata () =
  let macros = Language_macros.create () in
  let expand form = Option.get (List.find_map (fun macro -> macro form) macros) in
  let vector_meta = { source_meta with loc = { line = 3; column = 4 }; type_annotation = None } in
  let name = SAtom (source_meta, "v") in
  let value = parse_one "(source)" in
  let then_ = parse_one "(use v)" in
  let vector = SList (vector_meta, Bracket, [ name; value ]) in
  match expand (SList (call_meta, Paren, [ SAtom (call_meta, "if-let"); vector; then_ ])) with
  | SList (meta, Paren,
      [ SAtom (let_meta, "let*"); SList (pair_meta, Paren, [ n; v ]);
        SList (if_meta, Paren, [ SAtom (head_meta, "if"); test; t; SAtom (else_meta, "nil") ]) ]) ->
      Alcotest.(check bool) "binding list inherits vector metadata" true (pair_meta = vector_meta);
      Alcotest.(check bool) "generated nodes use call metadata" true
        (List.for_all (( = ) call_meta) [ meta; let_meta; if_meta; head_meta; else_meta ]);
      Alcotest.(check bool) "source nodes reused intact" true (n == name && v == value && test == name && t == then_)
  | form -> Alcotest.failf "expected let*/if expansion, got %s" (Frontend.show_sexpr form)

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
    [ "with-open"; "with-open-bindings"; "syntax-vector?"; "syntax-children";
      "syntax-simple-symbol?"; "syntax-list-at"; "macro-error" ]
  in
  let definitions = String.concat " " (List.map (fun name -> "(def " ^ name ^ " 99)") names) in
  let source = definitions ^ " (defn task [] (with-open [r (source)] 42)) (with-open [] 7)" in
  let eval context source =
    match Macros.Pipeline.parse_and_desugar source with
    | Ok forms -> Eval.eval_all ~context forms
    | Error message -> Alcotest.fail message
  in
  let context = Macros.Pipeline.create_context () in
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
  let other = Macros.Pipeline.create_context () in
  Alcotest.(check bool) "independent file" true (eval other "(with-open [] 42)" = [ Int 42 ]);
  Alcotest.(check bool) "previous definitions and helpers absent" true (!(other.namespaces) = [])

let boolean_macro_runtime_isolation macro () =
  let context = Macros.Pipeline.create_context () in
  let internal_name = macro in
  let condition = if macro = "and" then "true" else "false" in
  let source = Printf.sprintf "(def %s 99) (def gensym 7) (%s %s 42)" internal_name macro condition in
  let forms =
    match Macros.Pipeline.parse_and_desugar source with
    | Ok forms -> forms
    | Error message -> Alcotest.fail message
  in
  Alcotest.(check bool) "user names cannot replace internal definitions" true
    (Eval.eval_all ~context forms = [ Int 99; Int 7; Int 42 ]);
  Alcotest.(check bool) "only user names in runtime" true
    (!(context.namespaces) = [ ("user", [ ("gensym", Int 7); (internal_name, Int 99) ]) ]);
  let other = Macros.Pipeline.create_context () in
  ignore (Eval.eval_all ~context:other forms);
  Alcotest.(check bool) "new runtime starts without internal helpers" true
    (!(other.namespaces) = !(context.namespaces))

let shared_gensym () =
  let source = "(and (or false (and true 3)) (case (and true 7) 7 (and true 42) 0)) (and true 9) (or (or nil false) (and true 11))" in
  let forms =
    match Macros.Pipeline.parse_and_desugar source with
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
  Alcotest.(check int) "fresh across language macros" 10 (List.length (List.sort_uniq String.compare names));
  Alcotest.(check bool) "references resolve to their own binding" true (Eval.eval_all forms = [ Int 42; Int 9; Int 11 ])

let runtime_isolation () =
  let context = Macros.Pipeline.create_context () in
  let eval source =
    match Macros.Pipeline.parse_and_desugar source with
    | Ok forms -> Eval.eval_all ~context forms
    | Error message -> Alcotest.fail message
  in
  Alcotest.(check bool) "bootstrap leaves runtime empty" true (!(context.namespaces) = []);
  let result = eval "(def cond 99) (cond false 1 true cond)" in
  Alcotest.(check bool) "user binding cannot replace macro" true (result = [ Int 99; Int 99 ]);
  Alcotest.(check bool)
    "only user definitions in runtime" true
    (!(context.namespaces) = [ ("user", [ ("cond", Int 99) ]) ]);
  let other_context = Macros.Pipeline.create_context () in
  let forms =
    match Macros.Pipeline.parse_and_desugar "(cond true 42)" with
    | Ok forms -> forms
    | Error message -> Alcotest.fail message
  in
  Alcotest.(check bool)
    "independent source expands normally" true
    (Eval.eval_all ~context:other_context forms = [ Int 42 ]);
  Alcotest.(check bool) "internal and previous user names absent" true (!(other_context.namespaces) = [])

let prelude_path dir = List.fold_left Filename.concat dir [ "prelude"; "1.0.0"; "macro"; "prelude.clj" ]

let with_packages_dir dir f =
  let original = Sys.getenv "LY2K_PACKAGES_DIR" in
  Unix.putenv "LY2K_PACKAGES_DIR" dir;
  Fun.protect ~finally:(fun () -> Unix.putenv "LY2K_PACKAGES_DIR" original) f

let expand_with macros source = List.find_map (fun macro -> macro (parse_one source)) macros

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let without_packages_dir () =
  let env =
    Array.of_list
      (List.filter
         (fun entry -> not (String.starts_with ~prefix:"LY2K_PACKAGES_DIR=" entry))
         (Array.to_list (Unix.environment ())))
  in
  let run source =
    let program = "../bin/main.exe" in
    let ((stdout, stdin, stderr) as channels) = Unix.open_process_args_full program [| program |] env in
    output_string stdin source;
    close_out stdin;
    let output = In_channel.input_all stdout in
    let error = In_channel.input_all stderr in
    (Unix.close_process_full channels, output, error)
  in
  Alcotest.(check bool) "program without builtin macros runs" true (run {|(str "a" "b")|} = (Unix.WEXITED 0, "ab\n", ""));
  let status, _, error = run "(cond false 1 true 42)" in
  Alcotest.(check bool) "builtin macro fails naming the variable" true
    (status <> Unix.WEXITED 0 && contains error "LY2K_PACKAGES_DIR")

let missing_prelude_file () =
  let dir = Filename.temp_dir ~temp_dir:(Sys.getcwd ()) "packages" "" in
  with_packages_dir dir (fun () ->
      let macros = Language_macros.create () in
      Alcotest.check_raises "error names the expected path"
        (Eval_error ("cannot load builtin macros: " ^ prelude_path dir ^ ": No such file or directory"))
        (fun () -> ignore (expand_with macros "(cond true 1)")))

let incomplete_prelude_file content missing () =
  let dir = Filename.temp_dir ~temp_dir:(Sys.getcwd ()) "packages" "" in
  ignore
    (List.fold_left
       (fun parent name ->
         let path = Filename.concat parent name in
         Sys.mkdir path 0o755;
         path)
       dir [ "prelude"; "1.0.0"; "macro" ]);
  Out_channel.with_open_text (prelude_path dir) (fun channel -> output_string channel content);
  with_packages_dir dir (fun () ->
      let macros = Language_macros.create () in
      Alcotest.check_raises "missing definition fails on first expansion" (Eval_error ("symbol not found: " ^ missing))
        (fun () -> ignore (expand_with macros "(cond)")))

let () =
  Alcotest.run "language macros"
    [
      ( "bridge",
        [
          Alcotest.test_case "mixed tree metadata" `Quick mixed_tree_preserves_metadata;
          Alcotest.test_case "scalar categories" `Quick scalar_categories;
        ] );
      ( "prelude file",
        [ Alcotest.test_case "without packages dir" `Quick without_packages_dir;
          Alcotest.test_case "missing file" `Quick missing_prelude_file;
          Alcotest.test_case "missing definition" `Quick
            (incomplete_prelude_file
               {|(def cond (fn [clauses] nil)) (def and (fn [clauses] true)) (def or (fn [clauses] nil))|}
               "with-open");
          Alcotest.test_case "file without new definitions" `Quick
            (incomplete_prelude_file
               {|(def cond (fn [clauses] nil)) (def and (fn [clauses] true)) (def or (fn [clauses] nil))
                 (def with-open (fn [args] nil))|}
               "case") ] );
      ( "definitions",
        [ Alcotest.test_case "arbitrary name and helpers" `Quick custom_definition;
          Alcotest.test_case "call context" `Quick custom_call_context;
          Alcotest.test_case "state isolation" `Quick definition_state_isolation;
          Alcotest.test_case "recursion by form name" `Quick recursion_by_form_name ] );
      ( "or", [ Alcotest.test_case "language definition and metadata" `Quick language_or;
                 Alcotest.test_case "runtime isolation" `Quick (boolean_macro_runtime_isolation "or") ] );
      ( "with-open",
        [ Alcotest.test_case "structure and source metadata" `Quick language_with_open;
          Alcotest.test_case "form diagnostics" `Quick with_open_diagnostics;
          Alcotest.test_case "runtime isolation" `Quick with_open_runtime_isolation ] );
      ( "if-let", [ Alcotest.test_case "binding list metadata" `Quick language_if_let_metadata ] );
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
