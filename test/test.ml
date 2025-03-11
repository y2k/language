open Lib__.Common
module U = Utils

let _assert_js = Utils.assert_ (Lib.main_js true)
let _assert_java = Utils.assert_ (Lib.main_java "app" true)
let _assert_repl = Utils.assert_ (Lib.main_interpreter true)
let _assert_bytecode = Utils.assert_ (Lib.main_bytecode config_default true)

let () =
  Alcotest.run "Tests"
    [
      ( "Local",
        [
          _assert_java __POS__ {|(cast Number 1)|} "((Number)1)";
          _assert_java __POS__ {|(as 1 Number)|} "((Number)1)";
          (* *)
          _assert_bytecode __POS__ {|(ns m (:require ["./lib" :as bb])) (bb/f 3)|}
            "(\ndo*\n(\ndef*\nG3lib1f\n(\nfn*\n(\nx\n)\nx\n)\n)\n(\nG3lib1f\n3\n)\n)";
          _assert_bytecode __POS__ {|(ns m) (def bb)|} "(\ndef*\nG1m2bb\n)";
          _assert_bytecode __POS__ {|(ns m) (def bb 3)|} "(\ndef*\nG1m2bb\n3\n)";
          _assert_bytecode __POS__ {|(ns m) (defn bb [] 0)|} "(\ndef*\nG1m2bb\n(\nfn*\n(\n)\n0\n)\n)";
          _assert_bytecode __POS__ {|(ns m) (defn bb [] 0) (bb 3)|}
            "(\ndo*\n(\ndef*\nG1m2bb\n(\nfn*\n(\n)\n0\n)\n)\n(\nG1m2bb\n3\n)\n)";
        ] );
      ("Bytecode", U.make_samples_test (Lib.main_bytecode config_default true) "samples.bytecode");
      ("Repl", U.make_samples_test (Lib.main_interpreter true) "samples.repl");
      ("JS", U.make_samples_test (Lib.main_js true) "samples.js");
      ("Java", U.make_samples_test (Lib.main_java "app" true) "samples.java");
    ]
