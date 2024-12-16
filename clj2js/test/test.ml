module U = Utils

let _assert_js = Utils.assert_ (Lib.main_js true) "js/src/prelude.clj"
let _assert_java = Utils.assert_ (Lib.main_java true) "java/src/prelude.clj"

let () =
  Alcotest.run "Tests"
    [
      ( "Local",
        [
          _assert_java __POS__
            {|
(ns a.b.ab (:require [g.h.bar :as e]) (:require [c.d.foo :as f]))
(defn foo [] (f/dispatch 1 2))
|}
            {|package a.b.ab;
public class Main{
public static Object foo () {
return c.d.foo.dispatch(1, 2);
}
}|};
          _assert_java __POS__
            {|
(ns a.b.ab (:import [c.d Foo]))
(defn foo [] (Foo/dispatch 1 2))
|}
            {|package a.b.ab;
import c.d.Foo;
public class Main{
public static Object foo () {
return Foo.dispatch(1, 2);
}
}|};
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
