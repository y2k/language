open Core__.Common
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
    public static Object foo(Object a) { return a; }
}|}));
    let prelude_path =
      Sys.getenv "LY2K_PACKAGES_DIR" ^ "/prelude/1.0.0/java/prelude.clj"
    in
    Printf.sprintf
      "ly2k -src %s -target java -namespace y2k > %s/y2k/prelude.java"
      prelude_path dir
    |> Sys.command |> ignore;
    let temp_java_file = dir ^ "/y2k/" ^ namespace ^ ".java" in
    let java_code =
      FileReader.with_stub_scope ""
        (Backend_java.compile ~builtin_macro:Macro.invoke ~namespace:"y2k"
           ~log:true ~filename:temp_java_file)
        code
    in
    prerr_endline @@ "[temp java file]: " ^ temp_java_file;
    Out_channel.(
      with_open_bin temp_java_file (Fun.flip output_string java_code));
    Out_channel.(
      with_open_bin (dir ^ "/main.java")
        (Fun.flip output_string
           (Printf.sprintf
              {|
class main {
	public static void main(String[] args) throws Exception {
    System.exit(((Number) y2k.%s.test()).intValue());
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
    (* reify - anonymous class implementing interface *)
    ( __POS__,
      {|(defn test [] (.call (reify java.util.concurrent.Callable (call [this] 42))))|},
      "42" );
    (* reify with void method - use println which is a simple side effect *)
    ( __POS__,
      {|(defn test [] (let [a (atom 0)] (.run (reify Runnable (^void run [this] (println "hello")))) 42))|},
      "42" );
    (* reify with inner interface - $ should be converted to . in Java source *)
    ( __POS__,
      {|(defn test [] (let [h (reify java.lang.Thread$UncaughtExceptionHandler (^void uncaughtException [this ^Thread t ^Throwable e] (println "error")))] 42))|},
      "42" );
    (* Basic function tests *)
    (__POS__, {|(defn test [] 42)|}, {|42|});
    (__POS__, {|(defn test [] (if true 2 3))|}, {|2|});
    (__POS__, {|(defn test [] (if false 2 3))|}, {|3|});
    (__POS__, {|(defn test [] (if (= 2 2) 3 4))|}, {|3|});
    (__POS__, {|(defn test [] (if (= (str "1" "2" "3") "123") 2 3))|}, {|2|});
    (__POS__, {|(defn test [] (if (= (str 1 "2" 3) "123") 2 3))|}, {|2|});
    (* Function calls *)
    (__POS__, {|(defn f [] 3) (defn test [] (f))|}, {|3|});
    (__POS__, {|(defn f [x] x) (defn test [] (f 2))|}, {|2|});
    (__POS__, {|(defn f [x] x) (defn test [] (f 3) (f 2))|}, {|2|});
    (* Multiple arguments - need type hints for arithmetic *)
    ( __POS__,
      {|(defn f [^int a ^int b] (+ a b)) (defn test [] (f 40 2))|},
      {|42|} );
    (* Private functions *)
    (__POS__, {|(defn- f [{x :a}] x) (defn test [] (f {:a 2}))|}, {|2|});
    (__POS__, {|(defn- f [[_ x]] x) (defn test [] (f [3 2]))|}, {|2|});
    (__POS__, {|(def- a 42) (defn test [] a)|}, {|42|});
    (* Let bindings *)
    (__POS__, {|(defn test [] (let [a 40 b 2] (+ a b)))|}, {|42|});
    (* Anonymous functions (still use lambdas) *)
    (__POS__, {|(defn test [] ((fn [^int x] (+ x x)) 21))|}, {|42|});
    (__POS__, {|(defn test [] ((if true (fn [] 42) (fn [] 24))))|}, "42");
    (* Collections *)
    (__POS__, {|(defn test [] (count [10 20 30]))|}, {|3|});
    (__POS__, {|(defn test [] (count {:a 1 :b 2}))|}, {|2|});
    (__POS__, {|(defn test [] (get [10 20 30] 1))|}, {|20|});
    (__POS__, {|(defn test [] (get {:a 2 :b 3} :b))|}, {|3|});
    (__POS__, {|(defn test [] (:a {:a 42}))|}, "42");
    (__POS__, {|(defn test [] (count (conj [1 2] 3)))|}, "3");
    (* Interop *)
    (__POS__, {|(defn test [] (.hashCode "2"))|}, {|50|});
    (__POS__, {|(defn test [] (. "2" hashCode))|}, {|50|});
    (__POS__, {|(defn test [] (.hashCode (String. "2")))|}, {|50|});
    (__POS__, {|(defn test [] (.hashCode (new String "2")))|}, {|50|});
    (__POS__, {|(defn test [] (Integer/parseInt "2"))|}, {|2|});
    (* Type hints *)
    ( __POS__,
      {|(defn f [^int a] (str a) (+ a a))(defn test [] (f 3) (f 21))|},
      {|42|} );
    (* instanceof *)
    (__POS__, {|(defn test [] (if (instance? String "1") 2 3))|}, {|2|});
    (* Cast *)
    (__POS__, {|(defn test [] (let [x (int (double 8))]) x)|}, {|8|});
    (* Atom *)
    (__POS__, {|(defn test [] (let [x (atom 0)] (reset! x 42)))|}, {|42|});
    ( __POS__,
      {|(def a (atom 1)) (defn test [] (reset! a 2) (swap! a (fn [^int x] (+ x 1))) (deref a))|},
      {|3|} );
    (* Map function with lambda *)
    (__POS__, {|(defn test [] (count (map (fn [x] x) [1 2 3 4])))|}, "4");
    (* Higher-order with lambda *)
    ( __POS__,
      {|(defn f [g] ((:a g) 42)) (defn test [] (f {:a (fn [x] x)}))|},
      "42" );
    (* not= *)
    (__POS__, {|(defn test [] (if (not= 1 1) 2 3))|}, "3");
    (* unixtime *)
    ( __POS__,
      {|(defn test [] (if (and (> (unixtime) 1700000000) (< (unixtime) 2100000000)) 42 2))|},
      "42" );
    (* Destructuring *)
    ( __POS__,
      {|(defn test [] (let [[_ ^int a] [1 40 3] ^int b 2] (+ a b)))|},
      {|42|} );
    ( __POS__,
      {|(defn test4 [] (let [[y [x]] [0 [16]]] x)) (defn test [] (test4))|},
      "16" );
    (* gen-class with static method calls *)
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
    (* gen-class with :state and :init *)
    ( __POS__,
      {|(gen-class
           :name Counter
           :prefix "counter_"
           :init init
           :state state
           :methods [[getValue [] Object]])
          (defn counter_init [] (atom 40))
          (defn counter_getValue [^Counter self] (deref (.-state self)))
          (defn test [] (let [c (Counter.)] (swap! (.-state c) (fn [^int x] (+ x 2))) (.getValue c)))|},
      {|42|} );
    (* apply - call lambda with args from collection *)
    (__POS__, {|(defn test [] (apply (fn [^int x] (+ x 2)) [40]))|}, "42");
    ( __POS__,
      {|(defn test [] (apply (fn [^int a ^int b] (+ a b)) [40 2]))|},
      "42" );
    (__POS__, {|(defn test [] (apply (fn [] 42) []))|}, "42");
  ]
  |> JavaExecution.create_tests `Slow ~namespace:"user"
