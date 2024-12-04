module U = Utils

let _assert = Utils.assert_ (Lib.main_js true) "js/src/prelude.clj"
let _assert_java = Utils.assert_ (Lib.main_java true) "java/src/prelude.clj"

let () =
  Alcotest.run "Tests"
    [
      ("Local", [ (* _assert_java __POS__ {||} ""; *) ]);
      ( "Bytecode",
        U.make_samples_test (Lib.main_bytecode true) "bytecode/prelude.clj"
          "samples.bytecode" );
      ( "JS",
        U.make_samples_test (Lib.main_js true) "js/src/prelude.clj" "samples.js"
      );
      (* ( "JS - files",
         [
           assert_file __POS__ "hotreload-client.clj";
           assert_file __POS__ "sample1.clj";
           assert_file __POS__ "main.shared.clj";
         ] ); *)
      ( "Java",
        U.make_samples_test (Lib.main_java true) "java/src/prelude.clj"
          "samples.java" );
      (* ("Java - files", [ assert_file __POS__ "main.shared.clj" ]);  *)
    ]

(* let assert_ = Utils.assert_ (Lib.main_java true) "java/src/prelude.clj"
   let assert_file =
     Utils.assert_file (Lib.main_java true) "java/src/prelude.clj" ".java" *)
(* let assert_ = Utils.assert_ (Lib.main_js true) "js/src/prelude.clj"
   let assert_with_import =
     Utils.assert_with_import (Lib.main_js true) "js/src/prelude.clj"
   let assert_file =
     Utils.assert_file (Lib.main_js true) "js/src/prelude.clj" ".js" *)
