module U = Utils

let _assert_js = Utils.assert_ (Lib.main_js true) "js/src/prelude.clj"
let _assert_java = Utils.assert_ (Lib.main_java "app" true) "java/src/prelude.clj"

let () =
  Alcotest.run "Tests"
    [
      ("Local", []);
      ("Bytecode", U.make_samples_test (Lib.main_bytecode true) "bytecode/prelude.clj" "samples.bytecode");
      ("JS", U.make_samples_test (Lib.main_js true) "js/src/prelude.clj" "samples.js");
      ("Java", U.make_samples_test (Lib.main_java "app" true) "java/src/prelude.clj" "samples.java");
    ]
