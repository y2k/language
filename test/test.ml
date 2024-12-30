module U = Utils

let _assert_js = Utils.assert_ (Lib.main_js true) "js/src/prelude.clj"
let _assert_java = Utils.assert_ (Lib.main_java "app" true) "java/src/prelude.clj"
let _assert_repl = Utils.assert_ (Lib.main_interpreter true) "interpreter/prelude.clj"

let () =
  Alcotest.run "Tests"
    [
      ("Local", [
        _assert_repl __POS__ {|"he\nwo"|} "he\nwo";
        _assert_js __POS__ {|"he\nwo"|} {|"he\nwo"|};
      ]);
      ("Repl", U.make_samples_test (Lib.main_interpreter true) "interpreter/prelude.clj" "samples.repl");
      ("Bytecode", U.make_samples_test (Lib.main_bytecode true) "bytecode/prelude.clj" "samples.bytecode");
      ("JS", U.make_samples_test (Lib.main_js true) "js/src/prelude.clj" "samples.js");
      ("Java", U.make_samples_test (Lib.main_java "app" true) "java/src/prelude.clj" "samples.java");
    ]
