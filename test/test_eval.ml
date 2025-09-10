open Core__.Common
open Core__

module EvalExecution : sig
  val create_tests :
    [ `Slow | `Quick ] ->
    string ->
    (string * string * string) list ->
    unit Alcotest.test_case list
end = struct
  let create_tests speed path tests =
    tests
    |> List.map (fun (loc, input, expected) ->
           Alcotest.test_case loc speed (fun () ->
               let input = input ^ "\n(test)" in
               let actual =
                 FileReader.with_stub_scope "(defn foo [x] x)"
                   (Backend_eval.invoke ~builtin_macro:Macro.invoke true path)
                   input
               in
               Alcotest.(check string) "" expected actual))
end

let tests =
  [
    (__LOC__, {|(defn test [] (or nil nil 2 nil))|}, "2");
    (__LOC__, {|(defn test [] (if (some? (:b {:a 1 :b 2 :c 3})) 2 3))|}, "2");
    (__LOC__, {|(defn test [] (if (some? (:d {:a 1 :b 2 :c 3})) 2 3))|}, "3");
    ( __LOC__,
      {|(defn test [] (reduce (fn [a x] (str a "/" x)) (string/split "aa:bb:cc" ":")))|},
      "aa/bb/cc" );
    (__LOC__, {|(defn test [] (if (and true 1) 2 3))|}, "2");
    (__LOC__, {|(defn- f [x] x) (defn test [] (f 4))|}, "4");
    (__LOC__, {|(defn test [] (+ 2 2))|}, "4");
    ( __LOC__,
      {|(ns _ (:require ["./lib/eff" :as e])) (defn test [] (e/foo 4))|},
      "4" );
  ]
  |> EvalExecution.create_tests `Quick "/app/src/core/ext/user.clj"
