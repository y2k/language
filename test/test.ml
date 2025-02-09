open Lib__.Common
module U = Utils

let _assert_js = Utils.assert_ (Lib.main_js true)
let _assert_java = Utils.assert_ (Lib.main_java "app" true)
let _assert_repl = Utils.assert_ (Lib.main_interpreter true)
let _assert_bytecode = Utils.assert_ (Lib.main_bytecode config_default true)

let () =
  Alcotest.run "Tests"
    [
      ("Local", [
        (* _assert_js __POS__ {|(defn f [{a :a :as c b :b}] c)|} "" *)
      ]);
      ("Repl", U.make_samples_test (Lib.main_interpreter true) "samples.repl");
      ("Bytecode", U.make_samples_test (Lib.main_bytecode config_default true) "samples.bytecode");
      ("JS", U.make_samples_test (Lib.main_js true) "samples.js");
      ("Java", U.make_samples_test (Lib.main_java "app" true) "samples.java");
    ]
