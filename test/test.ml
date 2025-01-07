open Lib__.Common
module U = Utils

let _assert_js = Utils.assert_ (Lib.main_js true)
let _assert_java = Utils.assert_ (Lib.main_java "app" true)
let _assert_repl = Utils.assert_ (Lib.main_interpreter true)
let _assert_bytecode = Utils.assert_ (Lib.main_bytecode true)

let () =
  Alcotest.run "Tests"
    [
      ("Local", []);
      ("Repl", U.make_samples_test (Lib.main_interpreter true) "samples.repl");
      ( "Bytecode",
        U.make_samples_test
          (fun fname -> FileReader.with_stub_scope {|(ns lib) (defn f [x] x)|} (Lib.main_bytecode true fname))
          "samples.bytecode" );
      ("JS", U.make_samples_test (Lib.main_js true) "samples.js");
      ("Java", U.make_samples_test (Lib.main_java "app" true) "samples.java");
    ]
