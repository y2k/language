open Core__.Common
open Backend__

module EvalExecution = struct
  let run ~path input =
    let input = input ^ "\n(test)" in
    FileReader.with_stub_scope "(defn foo [x] x)"
      (Backend_eval.invoke ~builtin_macro:Macro.invoke true path)
      input

  let create_tests speed path tests =
    tests
    |> List.map (fun (pos, input, expected) ->
        Alcotest.test_case input speed (fun () ->
            let actual = run ~path input in
            Alcotest.(check ?pos:(Some pos) string) "" expected actual))
end

let tests =
  [
    (__POS__, {|(defn test [] (or nil nil 2 nil))|}, "2");
    (__POS__, {|(defn test [] (if (some? (:b {:a 1 :b 2 :c 3})) 2 3))|}, "2");
    (__POS__, {|(defn test [] (if (some? (:d {:a 1 :b 2 :c 3})) 2 3))|}, "3");
    ( __POS__,
      {|(defn test [] (reduce (fn [a x] (str a "/" x)) (string/split "aa:bb:cc" ":")))|},
      "aa/bb/cc" );
    (__POS__, {|(defn test [] (if (and true 1) 2 3))|}, "2");
    (__POS__, {|(defn- f [x] x) (defn test [] (f 4))|}, "4");
    (__POS__, {|(defn test [] (+ 2 2))|}, "4");
    ( __POS__,
      {|(ns _ (:require ["./lib/eff" :as e])) (defn test [] (e/foo 4))|},
      "4" );
  ]
  |> EvalExecution.create_tests `Quick "/app/src/core/ext/user.clj"
