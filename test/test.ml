open Lib__.Common
module U = Utils

let _assert_js = Utils.assert_ (Lib.main_js true)
let _assert_java = Utils.assert_ (Lib.main_java "app" true)
let _assert_repl = Utils.assert_ (Lib.main_interpreter true)
let _assert_bytecode = Utils.assert_ (Lib.main_bytecode { config_default with log = true })
let _assert_bytecode_repl = Utils.assert_ (Lib.main_bytecode { config_default with log = true; no_deps = true })

let () =
  Alcotest.run "Tests"
    [
      ( "Local",
        [
          _assert_java __POS__ {|(println (try (do (/ 2 0) (/ 1 0)) (catch Exception e (do (/ 3 0) nil))))|}
            "Object p__1;;\n\
             try {\n\
             (2/0);\n\
             p__1 = (1/0);\n\
             } catch (Exception e) {\n\
             (3/0);\n\
             p__1 = null;\n\
             };\n\
             y2k.RT.println(\n\
             p__1)";
          _assert_java __POS__ {|(println (try (/ 1 0) (catch Exception e nil)))|}
            "Object p__1;;\ntry {\np__1 = (1/0);\n} catch (Exception e) {\np__1 = null;\n};\ny2k.RT.println(\np__1)";
          (* *)
          _assert_java __POS__ {|(gen-class :annotations ["RW(AJU4::class)"] :name Tests)|}
            "@RW(AJU4::class)\npublic static class Tests extends Object {\npublic java.util.List<Object> state;\n}";
          _assert_java __POS__ {|(gen-class :name Tests)|}
            "public static class Tests extends Object {\npublic java.util.List<Object> state;\n}";
          (* *)
          _assert_java __POS__ {|(.al (.p "1") ^FL:void (fn [e] nil))|} "\"1\".p().al((FL)(e)->{\nnull;\n})";
          _assert_java __POS__ {|(unchecked! (.al (.p "1") ^FL:void (fn [e] nil)))|}
            "y2k.RT.try_(\ny2k.RT.fn(()->{\nreturn \"1\".p().al((FL)(e)->{\nnull;\n});\n}))";
          (* *)
          _assert_java __POS__ {|(drop 2 [1 2 3 4])|} "y2k.RT.drop(\n2,\njava.util.Arrays.asList(\n1,\n2,\n3,\n4))";
          _assert_java __POS__ {|(defn f [] ^a.v.V.OCL:void (fn [v] (.toString v)))|}
            "public static Object f () {\nreturn (a.v.V.OCL)(v)->{\nv.toString();\n};\n}";
          _assert_java __POS__ {|(defn f [] ^a.v.V.OCL (fn [v] (.toString v)))|}
            "public static Object f () {\nreturn (a.v.V.OCL)(v)->{\nreturn v.toString();\n};\n}";
          _assert_java __POS__ {|(cast Boolean nil)|} "((Boolean)null)";
          _assert_bytecode_repl __POS__ {|(ns bar (:require [lib :as ui])) (ui/a 1)|} "(\nG3lib1a\n1\n)";
          _assert_bytecode __POS__ {|(def f 2) true|} "(\ndo*\n(\ndef*\nG4user1f\n2\n)\ntrue\n)";
          _assert_bytecode __POS__ {|(def f 2) false|} "(\ndo*\n(\ndef*\nG4user1f\n2\n)\nfalse\n)";
          _assert_bytecode __POS__ {|(def f 2) nil|} "(\ndo*\n(\ndef*\nG4user1f\n2\n)\nnil\n)";
          _assert_bytecode __POS__ {|(def f 2) :f|} "(\ndo*\n(\ndef*\nG4user1f\n2\n)\n\"f\"\n)";
          _assert_bytecode __POS__ {|(def f 2) "f"|} "(\ndo*\n(\ndef*\nG4user1f\n2\n)\n\"f\"\n)";
          _assert_bytecode __POS__ {|(def f 2) (let [f 1] f)|}
            "(\ndo*\n(\ndef*\nG4user1f\n2\n)\n(\ndo*\n(\nlet*\nf\n1\n)\nf\n)\n)";
          _assert_bytecode __POS__ {|(def f 2) f|} "(\ndo*\n(\ndef*\nG4user1f\n2\n)\nG4user1f\n)";
          _assert_bytecode __POS__ {|(def a 2) (defn g [f] f)|}
            "(\ndo*\n(\ndef*\nG4user1a\n2\n)\n(\ndef*\nG4user1g\n(\nfn*\n(\nf\n)\nf\n)\n)\n)";
          _assert_bytecode __POS__ {|(def a 2) (defn g [f] f)|}
            "(\ndo*\n(\ndef*\nG4user1a\n2\n)\n(\ndef*\nG4user1g\n(\nfn*\n(\nf\n)\nf\n)\n)\n)";
          _assert_bytecode __POS__ {|(def f 2) (defn g [f] f)|}
            "(\ndo*\n(\ndef*\nG4user1f\n2\n)\n(\ndef*\nG4user1g\n(\nfn*\n(\nf\n)\nf\n)\n)\n)";
          _assert_bytecode __POS__ {|(defn f [x] x) (f 1)|}
            "(\ndo*\n(\ndef*\nG4user1f\n(\nfn*\n(\nx\n)\nx\n)\n)\n(\nG4user1f\n1\n)\n)";
          _assert_java __POS__ {|(spit "file.txt" "data")|} "y2k.RT.spit(\n\"file.txt\",\n\"data\")";
          _assert_java __POS__ {|(slurp "file.txt")|} "y2k.RT.slurp(\n\"file.txt\")";
          (* *)
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
      ("Bytecode", U.make_samples_test (Lib.main_bytecode { config_default with log = true }) "samples.bytecode");
      ("Repl", U.make_samples_test (Lib.main_interpreter true) "samples.repl");
      ("JS", U.make_samples_test (Lib.main_js true) "samples.js");
      ("Java", U.make_samples_test (Lib.main_java "app" true) "samples.java");
    ]
