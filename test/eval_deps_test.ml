module Eval = Backend_eval.Eval
module Pipeline = Backend_eval.Pipeline

let package_file source = { Eval.package_source = source }

let xml_package =
  {|(ns xml)

(defn- attrs_to_string [attrs]
  (reduce
   (fn [acc [k v]]
     (str acc " " k "='" v "'"))
   ""
   attrs))

(defn to-string [node]
  (if (vector? node)
    (let [tag (get node 0)]
      (if (= 1 (count node))
        (str "<" tag "></" tag ">")
        (if (= 2 (count node))
          (str "<" tag " " (attrs_to_string (get node 1)) ">"
               "</" tag ">")
          (str "<" tag " " (attrs_to_string (get node 1)) ">"
               (reduce
                (fn [a x] (str a (to-string x)))
                ""
                (drop 2 node))
               "</" tag ">"))))
    (str node)))|}

let eval_with_loader ?extensions loader input =
  match Pipeline.parse_and_desugar ?extensions input with
  | Ok sexprs ->
      let context = Pipeline.create_context ?extensions () in
      Eval.with_package_loader loader (fun () -> Eval.eval_all ~context sexprs)
  | Error message -> Alcotest.fail message

let last_string loader input =
  match List.rev (eval_with_loader loader input) with
  | Eval.String value :: _ -> value
  | _ -> Alcotest.fail "expected string result"

let loads_package_def () =
  let loader package version =
    match (package, version) with
    | "make", "0.1.0" -> [ package_file {|(def answer "42")|} ]
    | _ -> Alcotest.failf "unexpected package %s %s" package version
  in
  Alcotest.(check string) "result" "42" (last_string loader {|(deps {:make "0.1.0"}) answer|})

let loads_nested_deps () =
  let loader package version =
    match (package, version) with
    | "a", "0.1.0" -> [ package_file {|(deps {:b "0.1.0"}) (def from-a from-b)|} ]
    | "b", "0.1.0" -> [ package_file {|(def from-b "nested")|} ]
    | _ -> Alcotest.failf "unexpected package %s %s" package version
  in
  Alcotest.(check string) "result" "nested" (last_string loader {|(deps {:a "0.1.0"}) from-a|})

let extensions_in_nested_deps () =
  let creations = ref 0 in
  let extension () =
    incr creations;
    let count = ref 0 in
    [
      (function
      | Frontend.SList (meta, Paren, [ SAtom (_, "next") ]) ->
          incr count;
          Some (Frontend.SAtom (meta, string_of_int !count))
      | _ -> None);
    ]
  in
  let extensions = [ extension ] in
  let loader package version =
    match (package, version) with
    | "a", "0.1.0" ->
        [
          package_file {|(ns a) (def first (next)) (deps {:b "0.1.0"}) (def second (next))|};
          package_file {|(ns a) (def third (next))|};
        ]
    | "b", "0.1.0" -> [ package_file {|(ns b) (def first (next)) (def second (next))|} ]
    | _ -> Alcotest.failf "unexpected package %s %s" package version
  in
  let context = Pipeline.create_context ~extensions ~env:[ ("seed", Eval.String "seed") ] () in
  Alcotest.(check int) "context creation does not instantiate extensions" 0 !creations;
  let source =
    {|(ns app)
      (def local (next))
      (let [] (deps {:a "0.1.0"}))
      (def after (next))
      (str seed ":" local ":" after ":" a/first ":" a/second ":" a/third ":" b/first ":" b/second)|}
  in
  let forms =
    match Pipeline.parse_and_desugar ~extensions source with
    | Ok forms -> forms
    | Error message -> Alcotest.fail message
  in
  let values = Eval.with_package_loader loader (fun () -> Eval.eval_all ~context forms) in
  (match List.rev values with
  | Eval.String result :: _ ->
      Alcotest.(check string) "isolated file state and restored namespaces" "seed:1:2:1:2:1:1:2" result
  | _ -> Alcotest.fail "expected string result");
  Alcotest.(check int) "main and three package files" 4 !creations

let loads_at_form_position () =
  let loader package version =
    match (package, version) with
    | "make", "0.1.0" -> [ package_file {|(def answer "42")|} ]
    | _ -> Alcotest.failf "unexpected package %s %s" package version
  in
  Alcotest.check_raises "before deps" (Eval.Eval_error "symbol not found: answer") (fun () ->
      ignore (eval_with_loader loader {|answer (deps {:make "0.1.0"})|}));
  Alcotest.(check string) "after deps" "42" (last_string loader {|(def local "before") (deps {:make "0.1.0"}) answer|})

let language_cond_in_nested_deps () =
  let loader package version =
    match (package, version) with
    | "a", "0.1.0" ->
        [
          package_file {|(ns a) (deps {:b "0.1.0"}) (def answer (cond false 0 true b/answer))|};
          package_file {|(ns a) (def next (cond true (+ answer 1)))|};
        ]
    | "b", "0.1.0" -> [ package_file {|(ns b) (def answer (cond false 1 :else 42))|} ]
    | _ -> Alcotest.failf "unexpected package %s %s" package version
  in
  let source = {|(ns app) (def before 7) (deps {:a "0.1.0"}) (str before ":" a/answer ":" a/next)|} in
  Alcotest.(check string) "default extension and namespaces" "7:42:43" (last_string loader source);
  Alcotest.check_raises "nested package has no fallback" (Eval.Eval_error "symbol not found: cond") (fun () ->
      ignore (eval_with_loader ~extensions:[] loader source));
  Alcotest.(check string) "independent default after disabled call" "7:42:43" (last_string loader source)

let language_and_in_nested_deps () =
  let loader package version =
    match (package, version) with
    | "a", "0.1.0" ->
        [ package_file {|(ns a) (deps {:b "0.1.0"}) (def answer (and true b/answer))|} ]
    | "b", "0.1.0" -> [ package_file {|(ns b) (def expand-and 0) (def gensym 0) (def answer (and true 42))|} ]
    | _ -> Alcotest.failf "unexpected package %s %s" package version
  in
  let source = {|(ns app) (deps {:a "0.1.0"}) (str (and true a/answer))|} in
  Alcotest.(check string) "language and in both packages" "42" (last_string loader source);
  Alcotest.check_raises "nested package has no fallback" (Eval.Eval_error "symbol not found: and") (fun () ->
      ignore (eval_with_loader ~extensions:[] loader source));
  Alcotest.(check string) "independent default after disabled call" "42" (last_string loader source)

let language_or_in_nested_deps () =
  let loader package version =
    match (package, version) with
    | "a", "0.1.0" ->
        [ package_file {|(ns a) (deps {:b "0.1.0"}) (def answer (or nil b/answer))|} ]
    | "b", "0.1.0" -> [ package_file {|(ns b) (def expand-or 0) (def gensym 0) (def answer (or false 42))|} ]
    | _ -> Alcotest.failf "unexpected package %s %s" package version
  in
  let source = {|(ns app) (deps {:a "0.1.0"}) (str (or false a/answer))|} in
  Alcotest.(check string) "language or in both packages" "42" (last_string loader source);
  Alcotest.check_raises "nested package has no fallback" (Eval.Eval_error "symbol not found: or") (fun () ->
      ignore (eval_with_loader ~extensions:[] loader source));
  Alcotest.(check string) "independent default after disabled call" "42" (last_string loader source)

let language_with_open_in_nested_deps () =
  let loader package version =
    match (package, version) with
    | "a", "0.1.0" ->
        [ package_file {|(ns a) (deps {:b "0.1.0"}) (defn work [] (with-open [r (source)] (b/work)))|} ]
    | "b", "0.1.0" ->
        [ package_file {|(ns b) (defn work [] (with-open [r (source)] 42))|} ]
    | _ -> Alcotest.failf "unexpected package %s %s" package version
  in
  let parse ?extensions source =
    match Pipeline.parse_and_desugar ?extensions source with
    | Ok forms -> forms
    | Error message -> Alcotest.fail message
  in
  let check ?extensions enabled =
    let context = Pipeline.create_context ?extensions () in
    Eval.with_package_loader loader (fun () ->
        ignore (Eval.eval_all ~context (parse ?extensions {|(ns app) (deps {:a "0.1.0"})|})));
    List.iter
      (fun (name, result) ->
        let expected =
          if enabled then
            "(let [r (source)] (try (do " ^ result ^ ") (finally (.close (cast java.lang.AutoCloseable r)))))"
          else "(with-open (list r (source)) " ^ result ^ ")"
        in
        match Eval.find context name with
        | Eval.Closure (User (_, body, _, _)) ->
            Alcotest.(check (list string)) name
              (List.map Frontend.show_sexpr (parse ~extensions:[] expected))
              (List.map Frontend.show_sexpr body)
        | _ -> Alcotest.fail "expected unevaluated resource function")
      [ ("a/work", "(b/work)"); ("b/work", "42") ]
  in
  check true;
  check ~extensions:[] false;
  check true

let qualified_lookup_uses_alias_after_deps () =
  let loader package version =
    match (package, version) with
    | "make", "0.1.0" -> [ package_file {|(ns make) (def answer "42")|} ]
    | _ -> Alcotest.failf "unexpected package %s %s" package version
  in
  Alcotest.(check string)
    "result" "42"
    (last_string loader {|(ns app (:require [make :as m])) (deps {:make "0.1.0"}) m/answer|})

let renders_xml_manifest_package () =
  let loader package version =
    match (package, version) with
    | "xml", "0.3.0" -> [ package_file xml_package ]
    | _ -> Alcotest.failf "unexpected package %s %s" package version
  in
  let manifest =
    {|(ns manifest (:require [xml :as x]))
(deps {:xml "0.3.0"})
(x/to-string
 [:manifest {:xmlns:android "http://schemas.android.com/apk/res/android"}
  [:application {:android:icon "@drawable/ic_launcher"
                 :android:label "Interpreter"
                 :android:roundIcon "@drawable/ic_launcher"
                 :android:theme "@android:style/Theme.Material.NoActionBar"}
   [:activity {:android:name "io.github.y2k.main$MainActivity"
               :android:configChanges "orientation|screenSize"
               :android:exported "true"}
    [:intent-filter {}
     [:action {:android:name "android.intent.action.MAIN"}]
     [:category {:android:name "android.intent.category.LAUNCHER"}]]]]])|}
  in
  let expected =
    "<manifest  xmlns:android='http://schemas.android.com/apk/res/android'>"
    ^ "<application  android:icon='@drawable/ic_launcher' android:label='Interpreter' \
       android:roundIcon='@drawable/ic_launcher' android:theme='@android:style/Theme.Material.NoActionBar'>"
    ^ "<activity  android:name='io.github.y2k.main$MainActivity' android:configChanges='orientation|screenSize' \
       android:exported='true'>" ^ "<intent-filter ><action  android:name='android.intent.action.MAIN'></action>"
    ^ "<category  \
       android:name='android.intent.category.LAUNCHER'></category></intent-filter></activity></application></manifest>"
  in
  Alcotest.(check string) "result" expected (last_string loader manifest)

let rejects_non_string_pairs () =
  List.iter
    (fun input ->
      Alcotest.check_raises input (Eval.Eval_error "deps expects string package/version pairs") (fun () ->
          ignore (eval_with_loader (fun _ _ -> Alcotest.fail "invalid pair reached loader") input)))
    [
      "(deps {:package 1})";
      "(deps {1 \"version\"})";
      "(deps (hash-map 'package \"version\"))";
      "(deps (hash-map \"package\" 'version))";
    ]

let () =
  Alcotest.run "eval deps"
    [
      ( "deps",
        [
          Alcotest.test_case "loads package def" `Quick loads_package_def;
          Alcotest.test_case "loads nested deps" `Quick loads_nested_deps;
          Alcotest.test_case "extensions in nested deps" `Quick extensions_in_nested_deps;
          Alcotest.test_case "language cond in nested deps" `Quick language_cond_in_nested_deps;
          Alcotest.test_case "language and in nested deps" `Quick language_and_in_nested_deps;
          Alcotest.test_case "language or in nested deps" `Quick language_or_in_nested_deps;
          Alcotest.test_case "language with-open in nested deps" `Quick language_with_open_in_nested_deps;
          Alcotest.test_case "loads at form position" `Quick loads_at_form_position;
          Alcotest.test_case "qualified lookup uses alias after deps" `Quick qualified_lookup_uses_alias_after_deps;
          Alcotest.test_case "renders xml manifest package" `Quick renders_xml_manifest_package;
          Alcotest.test_case "rejects non-string package/version" `Quick rejects_non_string_pairs;
        ] );
    ]
