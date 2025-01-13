open Lib__.Common
module U = Utils

let _assert_js = Utils.assert_ (Lib.main_js true)
let _assert_java = Utils.assert_ (Lib.main_java "app" true)
let _assert_repl = Utils.assert_ (Lib.main_interpreter true)
let _assert_bytecode = Utils.assert_ (Lib.main_bytecode true)

let () =
  Alcotest.run "Tests"
    [
      ("Local", [
        (* _assert_repl __POS__ {|
        (defn f [xs]
          (if (vector? xs)
            (reduce (fn [a x] (+ a (f x))) 0 xs)
            xs))
        (f [1 [11 12] 2])|} ""; *)
        (* _assert_repl __POS__ {|(defn f [a b] (println 3) (if a (f false 2) b)) (f true 1)|} ""; *)
        _assert_repl __POS__ {|
(defn to_string [node] (let [[tag] node] (if (= 1 (count node)) tag (if (map? (get node 1)) nil (str tag (reduce (fn [a x] (str a (to_string x))) "" (drop 1 node)) tag))))) (to_string [:A [:B]])
|} "ABA";
      ]);
      ("Repl", U.make_samples_test (Lib.main_interpreter true) "samples.repl");
      ( "Bytecode",
        U.make_samples_test
          (fun fname -> FileReader.with_stub_scope {|(ns lib) (defn f [x] x)|} (Lib.main_bytecode true fname))
          "samples.bytecode" );
      ("JS", U.make_samples_test (Lib.main_js true) "samples.js");
      ("Java", U.make_samples_test (Lib.main_java "app" true) "samples.java");
    ]
