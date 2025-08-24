open Core__.Common
open Core__

module JavaExecution : sig
  val create_tests :
    [ `Slow | `Quick ] ->
    string ->
    string ->
    (string * string * string) list ->
    unit Alcotest.test_case list
end = struct
  let run cls_name (code : string) =
    (* prerr_endline @@ "DIR: " ^ Sys.getcwd (); *)
    let prelude =
      In_channel.(with_open_bin "../../../prelude/data/y2k/RT.java" input_all)
      |> Str.global_replace (Str.regexp "package y2k;") ""
      |> Str.global_replace (Str.regexp "public class RT") "class RT"
      |> Str.global_replace (Str.regexp "import java\\.util\\.\\*;\n") ""
    in
    let code =
      code
      |> Str.global_replace (Str.regexp "package .+;\n") ""
      |> Str.global_replace (Str.regexp "core\\.ext\\.lib\\.") ""
      |> Str.global_replace (Str.regexp "y2k\\.root\\.") ""
    in
    let code =
      Printf.sprintf
        {|package y2k;

import java.util.*;

public class App {
%s;

public static void main(String[] args) {System.exit((int) RT.invoke(%s.test));}}%s|}
        code cls_name prelude
    in
    prerr_endline code;
    let file_name = Filename.temp_file "y2k" ".java" in
    Out_channel.(with_open_bin file_name (fun f -> output_string f code));
    let result = Sys.command (Printf.sprintf "java %s" file_name) in
    string_of_int result

  let create_tests speed path cls_name tests =
    tests
    |> List.map (fun (loc, input, expected) ->
           Alcotest.test_case loc speed (fun () ->
               let ext_module =
                 {|(ns _) (defn foo [x] x)|}
                 |> Backend_java.compile "lib" false
                      "/app/src/core/ext/lib/eff.clj" "/app/src"
               in
               let code =
                 FileReader.with_stub_scope ""
                   (Backend_java.compile "y2k.root" true path "/app/src")
                   input
               in
               let actual = ext_module ^ "\n" ^ code |> run cls_name in
               Alcotest.(check string) "" expected actual))
end

let tests =
  [
    ( __LOC__,
      {|
    (declare parse)
    (defn test6 [] (if (= 3 (count (take 3 [1 2 3 4 5 6]))) 64 0))
    (defn test5 [] (if (= [3 2 1] (shuffle 0.0 [1 2 3])) 32 0))
    (defn test4 [] (let [[y [x]] [0 [16]]] x))
    (defn test3 [] (let [x (int (double 8))]) x)
    (defn test2 [] (let [x (int (count [1 2 3])) ^int y (+ 1 x)]) 4)
    (defn test1 [] (if (= 5 (mod 33 7)) 2 0))
    (defn test [] (+ 1 (int (test6)) (int (test5)) (int (test4)) (int (test3)) (int (test2)) (int (test1))))|},
      "127" );
    (* Atom *)
    (__LOC__, {|(defn test [] (let [x (atom 0)] (reset! x 42)))|}, {|42|});
    (* *)
    (__LOC__, {|(defn test [] ((fn [^int x] (+ x x)) 21))|}, {|42|});
    (* *)
    ( __LOC__,
      {|(defn test [] (let [[_ ^int a] [1 40 3] ^int b 2] (+ a b)))|},
      {|42|} );
    (__LOC__, {|(defn test [] (let [a 40 b 2] (+ a b)))|}, {|42|});
    (* *)
    ( __LOC__,
      {|(defn test [] (if (and (> (unixtime) 1700000000) (< (unixtime) 2100000000)) 42 2))|},
      "42" );
    (__LOC__, {|(defn test [] (if (not= 1 1) 2 3))|}, "3");
    (__LOC__, {|(defn test [] (count (conj [1 2] 3)))|}, "3");
    ( __LOC__,
      {|(ns _ (:require ["./lib/eff" :as e])) (defn test [] (e/foo 42))|},
      {|42|} );
    ( __LOC__,
      {|(defn test [] (java.util.Objects.requireNonNull 42 ^java.util.function.Supplier (fn [] "")))|},
      "42" );
    (__LOC__, {|(defn test [] (count (map (fn [x] x) [1 2 3 4])))|}, "4");
    (__LOC__, {|(defn test [] (if (instance? String "1") 2 3))|}, {|2|});
    (* *)
    (__LOC__, {|(defn test [] ((if true (fn [] 42) (fn [] 24))))|}, "42");
    ( __LOC__,
      {|(defn f [g] ((:a g) 42)) (defn test [] (f {:a (fn [x] x)}))|},
      "42" );
    (__LOC__, {|(defn test [] (:a {:a 42}))|}, "42");
    ( __LOC__,
      {|(ns _ (:import [java.util Map])) (defn f [^Map x] 42) (defn test [] (f nil))|},
      {|42|} );
    ( __LOC__,
      {|(defn f [^int a] (str a) (+ a a))(defn test [] (f 3) (f 21))|},
      {|42|} );
    ( __LOC__,
      {|(ns _ (:import [java.util Map]))
          (defn test []
            (let [^Map db nil]
              42))
          |},
      {|42|} );
    (* *)
    (__LOC__, {|(defn test [] (Integer/parseInt "2"))|}, {|2|});
    (* *)
    (__LOC__, {|(defn test [] (. "2" hashCode))|}, {|50|});
    (__LOC__, {|(defn test [] (.hashCode "2"))|}, {|50|});
    (* *)
    ( __LOC__,
      {|(ns _ (:import [java.util Date])) (defn test [] (.hashCode (Date. 42)))|},
      {|42|} );
    (__LOC__, {|(def- a 42) (defn test [] a)|}, {|42|});
    (* *)
    (__LOC__, {|(defn f [] 3) (defn test [] (f))|}, {|3|});
    ( __LOC__,
      {|(def a (atom 1)) (defn test [] (reset! a 2) (swap! a (fn [^int x] (+ x 1))) (deref a))|},
      {|3|} );
    (__LOC__, {|(defn- f [{x :a}] x) (defn test [] (f {:a 2}))|}, {|2|});
    (__LOC__, {|(defn- f [[_ x]] x) (defn test [] (f [3 2]))|}, {|2|});
    (__LOC__, {|(defn f [x] x) (defn test [] (f 2))|}, {|2|});
    (__LOC__, {|(defn f [x] x) (defn test [] (f 3) (f 2))|}, {|2|});
    ( __LOC__,
      {|(defn f [x] (if (= (str "1" x "3") "123") 2 3)) (defn test [] (f 2))|},
      {|2|} );
    (* *)
    ( __LOC__,
      {|(gen-class
           :name MyClass
           :extends Thread
           :prefix "sample_"
           :methods [[add [int int] int]
                     [^:static foo1 ["String[]"] Object]
                     [^:static foo2 [] void]])
          (defn sample_foo1 [_] nil)
          (defn sample_foo2 [] nil)
          (defn sample_add [self ^int a ^int b] (+ a b))
          (defn test [] (.add (MyClass.) 40 2))|},
      {|42|} );
    (* *)
    (__LOC__, {|(defn test [] (.hashCode (new String "2")))|}, {|50|});
    (__LOC__, {|(defn test [] (.hashCode (String. "2")))|}, {|50|});
    (* *)
    (__LOC__, {|(defn test [] (count [10 20 30]))|}, {|3|});
    (__LOC__, {|(defn test [] (count {:a 1 :b 2}))|}, {|2|});
    (__LOC__, {|(defn test [] (get [10 20 30] 1))|}, {|20|});
    (__LOC__, {|(defn test [] (get {:a 2 :b 3} :b))|}, {|3|});
    (__LOC__, {|(defn test [] (if (= (str "1" "2" "3") "123") 2 3))|}, {|2|});
    (__LOC__, {|(defn test [] (if (= (str 1 "2" 3) "123") 2 3))|}, {|2|});
    (__LOC__, {|(defn test [] (if (= 2 2) 3 4))|}, {|3|});
    (__LOC__, {|(defn test [] (if false 2 3))|}, {|3|});
    (__LOC__, {|(defn test [] (if true 2 3))|}, {|2|});
  ]
  |> JavaExecution.create_tests `Slow "/app/src/core/ext/user.clj" "user"
