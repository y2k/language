module U = Utils

let _assert_js = Utils.assert_ (Lib.main_js true) "js/src/prelude.clj"
let _assert_java = Utils.assert_ (Lib.main_java true) "java/src/prelude.clj"

let () =
  Alcotest.run "Tests"
    [
      ( "Local",
        [
          (* _assert_java __POS__ {||} ""; *)
          (*  *)
          _assert_java __POS__ {|(class JobScheduler)|} "JobScheduler.class";
          _assert_java __POS__ {|(let [^A a (foo 1)] 2)|} "A a = foo(1);;\n2";
          _assert_java __POS__ {|(as 1 String)|} "((String)1)";
          _assert_java __POS__ {|(is 1 String)|} "(1 instanceof String)";
          (* *)
          _assert_java __POS__ {|(def- a 2)(set! a.bar 1)|}
            "private static Object a=2;;\na.bar = 1;";
          _assert_js __POS__ {|(def- a 2)(set! a.bar 1)|}
            "const a = 2;\na.bar = 1;";
          _assert_java __POS__ {|(assoc {} :k :v)|}
            "y2k.RT.assoc(y2k.RT.hash_map(), \"k\", \"v\")";
          (* *)
          _assert_js __POS__ {|(merge {:f (fn [{u :a}] 2)} 3)|} "";
        ] );
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
