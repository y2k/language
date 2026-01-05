open Core__.Common
open Core__
open Backend__

module JavaExecution = struct
  let run_code ~namespace code =
    let dir = Filename.temp_dir "ly2k" "test" in
    prerr_endline @@ "DIR: " ^ dir;
    Sys.command (Printf.sprintf "mkdir %s/y2k" dir) |> ignore;
    Out_channel.(
      with_open_bin (dir ^ "/y2k/ff.java")
        (Fun.flip output_string
           {|
package y2k;
public class ff {
    public static Object foo = y2k.RT.fn((a) -> a);
}|}));
    Out_channel.(
      with_open_bin (dir ^ "/y2k/RT.java")
        (Fun.flip output_string Prelude.java_runtime));
    Out_channel.(
      with_open_bin
        (dir ^ "/y2k/prelude_java.java")
        (Fun.flip output_string Prelude.java_runtime2));
    let java_code =
      FileReader.with_stub_scope ""
        (Backend_java.compile ~builtin_macro:Macro.invoke ~namespace:"y2k" true
           "app/test")
        code
    in
    Out_channel.(
      with_open_bin
        (dir ^ "/y2k/" ^ namespace ^ ".java")
        (Fun.flip output_string java_code));
    Out_channel.(
      with_open_bin (dir ^ "/main.java")
        (Fun.flip output_string
           (Printf.sprintf
              {|
class main {
	public static void main(String[] args) {
    System.exit((int) y2k.RT.invoke(y2k.%s.test));
	}
}|}
              namespace)));
    Sys.command (Printf.sprintf "cd %s; java main.java" dir) |> string_of_int

  let create_tests speed ~namespace tests =
    tests
    |> List.map (fun (pos, input, expected) ->
        Alcotest.test_case input speed (fun () ->
            let actual = run_code ~namespace input in
            Alcotest.(check ?pos:(Some pos) string) "" expected actual))
end

let tests =
  [
    ( __POS__,
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
    (__POS__, {|(defn test [] (let [x (atom 0)] (reset! x 42)))|}, {|42|});
    (__POS__, {|(defn test [] ((fn [^int x] (+ x x)) 21))|}, {|42|});
    ( __POS__,
      {|(defn test [] (let [[_ ^int a] [1 40 3] ^int b 2] (+ a b)))|},
      {|42|} );
    (__POS__, {|(defn test [] (let [a 40 b 2] (+ a b)))|}, {|42|});
    ( __POS__,
      {|(defn test [] (if (and (> (unixtime) 1700000000) (< (unixtime) 2100000000)) 42 2))|},
      "42" );
    (__POS__, {|(defn test [] (if (not= 1 1) 2 3))|}, "3");
    (__POS__, {|(defn test [] (count (conj [1 2] 3)))|}, "3");
    ( __POS__,
      {|(defn test [] (java.util.Objects.requireNonNull 42 ^java.util.function.Supplier (fn [] "")))|},
      "42" );
    (__POS__, {|(defn test [] (count (map (fn [x] x) [1 2 3 4])))|}, "4");
    (__POS__, {|(defn test [] (if (instance? String "1") 2 3))|}, {|2|});
    (__POS__, {|(defn test [] ((if true (fn [] 42) (fn [] 24))))|}, "42");
    ( __POS__,
      {|(defn f [g] ((:a g) 42)) (defn test [] (f {:a (fn [x] x)}))|},
      "42" );
    (__POS__, {|(defn test [] (:a {:a 42}))|}, "42");
    ( __POS__,
      {|(ns _ (:import [java.util Map])) (defn f [^Map x] 42) (defn test [] (f nil))|},
      {|42|} );
    ( __POS__,
      {|(defn f [^int a] (str a) (+ a a))(defn test [] (f 3) (f 21))|},
      {|42|} );
    ( __POS__,
      {|(ns _ (:import [java.util Map]))
          (defn test []
            (let [^Map db nil]
              42))
          |},
      {|42|} );
    (__POS__, {|(defn test [] (Integer/parseInt "2"))|}, {|2|});
    (__POS__, {|(defn test [] (. "2" hashCode))|}, {|50|});
    (__POS__, {|(defn test [] (.hashCode "2"))|}, {|50|});
    ( __POS__,
      {|(ns _ (:import [java.util Date])) (defn test [] (.hashCode (Date. 42)))|},
      {|42|} );
    (__POS__, {|(def- a 42) (defn test [] a)|}, {|42|});
    (__POS__, {|(defn f [] 3) (defn test [] (f))|}, {|3|});
    ( __POS__,
      {|(def a (atom 1)) (defn test [] (reset! a 2) (swap! a (fn [^int x] (+ x 1))) (deref a))|},
      {|3|} );
    (__POS__, {|(defn- f [{x :a}] x) (defn test [] (f {:a 2}))|}, {|2|});
    (__POS__, {|(defn- f [[_ x]] x) (defn test [] (f [3 2]))|}, {|2|});
    (__POS__, {|(defn f [x] x) (defn test [] (f 2))|}, {|2|});
    (__POS__, {|(defn f [x] x) (defn test [] (f 3) (f 2))|}, {|2|});
    ( __POS__,
      {|(defn f [x] (if (= (str "1" x "3") "123") 2 3)) (defn test [] (f 2))|},
      {|2|} );
    ( __POS__,
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
    (__POS__, {|(defn test [] (.hashCode (new String "2")))|}, {|50|});
    (__POS__, {|(defn test [] (.hashCode (String. "2")))|}, {|50|});
    (__POS__, {|(defn test [] (count [10 20 30]))|}, {|3|});
    (__POS__, {|(defn test [] (count {:a 1 :b 2}))|}, {|2|});
    (__POS__, {|(defn test [] (get [10 20 30] 1))|}, {|20|});
    (__POS__, {|(defn test [] (get {:a 2 :b 3} :b))|}, {|3|});
    (__POS__, {|(defn test [] (if (= (str "1" "2" "3") "123") 2 3))|}, {|2|});
    (__POS__, {|(defn test [] (if (= (str 1 "2" 3) "123") 2 3))|}, {|2|});
    (__POS__, {|(defn test [] (if (= 2 2) 3 4))|}, {|3|});
    (__POS__, {|(defn test [] (if false 2 3))|}, {|3|});
    (__POS__, {|(defn test [] (if true 2 3))|}, {|2|});
  ]
  |> JavaExecution.create_tests `Slow ~namespace:"user"
