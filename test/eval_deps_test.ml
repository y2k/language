module Eval = Backend_eval.Eval

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

let eval_with_loader loader input =
  match Frontend.parse_and_desugar input with
  | Ok sexprs -> Eval.with_package_loader loader (fun () -> Eval.eval_all sexprs)
  | Error message -> Alcotest.fail message

let last_symbol loader input =
  match List.rev (eval_with_loader loader input) with
  | Eval.Symbol value :: _ -> value
  | _ -> Alcotest.fail "expected symbol result"

let loads_package_def () =
  let loader package version =
    match (package, version) with
    | "make", "0.1.0" -> [ package_file {|(def answer "42")|} ]
    | _ -> Alcotest.failf "unexpected package %s %s" package version
  in
  Alcotest.(check string) "result" "42" (last_symbol loader {|(deps {:make "0.1.0"}) answer|})

let loads_nested_deps () =
  let loader package version =
    match (package, version) with
    | "a", "0.1.0" -> [ package_file {|(deps {:b "0.1.0"}) (def from-a from-b)|} ]
    | "b", "0.1.0" -> [ package_file {|(def from-b "nested")|} ]
    | _ -> Alcotest.failf "unexpected package %s %s" package version
  in
  Alcotest.(check string) "result" "nested" (last_symbol loader {|(deps {:a "0.1.0"}) from-a|})

let loads_at_form_position () =
  let loader package version =
    match (package, version) with
    | "make", "0.1.0" -> [ package_file {|(def answer "42")|} ]
    | _ -> Alcotest.failf "unexpected package %s %s" package version
  in
  Alcotest.check_raises "before deps" (Eval.Eval_error "symbol not found: answer") (fun () ->
      ignore (eval_with_loader loader {|answer (deps {:make "0.1.0"})|}));
  Alcotest.(check string) "after deps" "42" (last_symbol loader {|(def local "before") (deps {:make "0.1.0"}) answer|})

let qualified_lookup_uses_alias_after_deps () =
  let loader package version =
    match (package, version) with
    | "make", "0.1.0" -> [ package_file {|(ns make) (def answer "42")|} ]
    | _ -> Alcotest.failf "unexpected package %s %s" package version
  in
  Alcotest.(check string)
    "result" "42"
    (last_symbol loader {|(ns app (:require [make :as m])) (deps {:make "0.1.0"}) m/answer|})

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
  Alcotest.(check string) "result" expected (last_symbol loader manifest)

let () =
  Alcotest.run "eval deps"
    [
      ( "deps",
        [
          Alcotest.test_case "loads package def" `Quick loads_package_def;
          Alcotest.test_case "loads nested deps" `Quick loads_nested_deps;
          Alcotest.test_case "loads at form position" `Quick loads_at_form_position;
          Alcotest.test_case "qualified lookup uses alias after deps" `Quick qualified_lookup_uses_alias_after_deps;
          Alcotest.test_case "renders xml manifest package" `Quick renders_xml_manifest_package;
        ] );
    ]
