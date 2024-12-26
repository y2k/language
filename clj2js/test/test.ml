module U = Utils

let _assert_js = Utils.assert_ (Lib.main_js true) "js/src/prelude.clj"
let _assert_java = Utils.assert_ (Lib.main_java "app" true) "java/src/prelude.clj"

let () =
  Alcotest.run "Tests"
    [
      ( "Local",
        [
          _assert_java __POS__ {|(fn [x] (x 1))|} "y2k.RT.fn((x)->{\nreturn y2k.RT.invoke(x, 1);\n})";
          _assert_java __POS__ {|(declare g) (defn f [] (g ^Runnable (fn [] 1)))|}
            "public static Object f () {\nreturn g((Runnable)()->{\nreturn 1;\n});\n}";
          _assert_java __POS__ {|(declare g) (defn f [] (g (fn [] 1)))|}
            "public static Object f () {\nreturn g(y2k.RT.fn(()->{\nreturn 1;\n}));\n}";
          _assert_java __POS__ {|(declare g) (def f (g 1))|} "public static Object f=g(1);";
          _assert_java __POS__ {|(def f (m/g 1))|} "public static Object f=m.g(1);";
        ] );
      ("Bytecode", U.make_samples_test (Lib.main_bytecode true) "bytecode/prelude.clj" "samples.bytecode");
      ("JS", U.make_samples_test (Lib.main_js true) "js/src/prelude.clj" "samples.js");
      ("Java", U.make_samples_test (Lib.main_java "app" true) "java/src/prelude.clj" "samples.java");
    ]
