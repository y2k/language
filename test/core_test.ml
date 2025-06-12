module JavaExecution : sig
  val create_tests : (string * string * string) list -> unit Alcotest.test_case list
end = struct
  let run (code : string) =
    (* prerr_endline @@ "DIR: " ^ Sys.getcwd (); *)
    let prelude =
      In_channel.(with_open_bin "../../../y2k/RT.java" input_all)
      |> Str.global_replace (Str.regexp "package y2k;") ""
      |> Str.global_replace (Str.regexp "public class RT") "class RT"
    in
    let code =
      code
      |> Str.global_replace (Str.regexp "package .+;\n") ""
      |> Str.global_replace (Str.regexp "core\\.ext\\.lib\\.") ""
    in
    let code =
      Printf.sprintf
        {|package y2k;

public class App {
%s;

public static void main(String[] args) {System.exit((int) RT.invoke(User.run));}}%s|}
        code prelude
    in
    prerr_endline code;
    let file_name = Filename.temp_file "y2k" ".java" in
    Out_channel.(with_open_bin file_name (fun f -> output_string f code));
    let result = Sys.command (Printf.sprintf "java %s" file_name) in
    string_of_int result

  let create_tests tests =
    tests
    |> List.map (fun (loc, input, expected) ->
           Alcotest.test_case loc `Quick (fun () ->
               let ext_module =
                 {|(ns _) (defn foo [x] x)|} |> Core.compile "lib" false "/app/src/core/ext/lib/eff.clj" "/app/src"
               in
               let code = Core.compile "user" true "/app/src/core/ext/user.clj" "/app/src" input in
               let actual = ext_module ^ "\n" ^ code |> run in
               Alcotest.(check string) "" expected actual))
end

module EvalExecution : sig
  val create_tests : (string * string * string) list -> unit Alcotest.test_case list
end = struct
  open Lib__.Common

  let create_tests tests =
    tests
    |> List.map (fun (loc, input, expected) ->
           Alcotest.test_case loc `Quick (fun () ->
               let actual =
                 FileReader.with_stub_scope "(defn foo [x] x)" (Core.eval "/app/src/core/ext/user.clj" "") input
               in
               Alcotest.(check string) "" expected actual))
end

let tests =
  [
    [
      (__LOC__, {|(defn- f [x] x) (f 4)|}, "4");
      (__LOC__, {|(+ 2 2)|}, "4");
      (__LOC__, {|(ns _ (:require ["./lib/eff" :as e])) (e/foo 4)|}, "4");
    ]
    |> EvalExecution.create_tests;
    [
      (__LOC__, {|(defn run [] (Integer/parseInt "2"))|}, {|2|});
      (__LOC__, {|(defn run [] (. "2" hashCode))|}, {|50|});
      (__LOC__, {|(defn run [] (.hashCode "2"))|}, {|50|});
      (* *)
      (__LOC__, {|(ns _ (:require ["./lib/eff" :as e])) (defn run [] (e/foo 42))|}, {|42|});
      (__LOC__, {|(ns _ (:import [java.util Date])) (defn run [] (.hashCode (Date. 42)))|}, {|42|});
      (__LOC__, {|(def- a 42) (defn run [] a)|}, {|42|});
      (* *)
      (__LOC__, {|(defn f [] 3) (defn run [] (f))|}, {|3|});
      (__LOC__, {|(def a (atom 1)) (defn run [] (reset! a 2) (swap! a (fn [x] (+ x 1))) (deref a))|}, {|3|});
      (__LOC__, {|(defn- f [{x :a}] x) (defn run [] (f {:a 2}))|}, {|2|});
      (__LOC__, {|(defn- f [[_ x]] x) (defn run [] (f [3 2]))|}, {|2|});
      (__LOC__, {|(defn f [x] x) (defn run [] (f 2))|}, {|2|});
      (__LOC__, {|(defn f [x] x) (defn run [] (f 3) (f 2))|}, {|2|});
      (__LOC__, {|(defn f [x] (if (= (str "1" x "3") "123") 2 3)) (defn run [] (f 2))|}, {|2|});
      (* *)
      (__LOC__, {|(defn run [] (let [[_ a] [1 40 3] b 2] (+ a b)))|}, {|42|});
      (__LOC__, {|(defn run [] (let [a 40 b 2] (+ a b)))|}, {|42|});
      (* *)
      ( __LOC__,
        {|(gen-class
 :name MyClass
 :extends Thread
 :prefix "sample_"
 :methods [[add [int int] int]])

(defn sample_add [self a b]
  (+ a b))

(defn run []
  (.add (MyClass.) 40 2))|},
        {|42|} );
      (* *)
      (__LOC__, {|(defn run [] ((fn [x] (+ x x)) 21))|}, {|42|});
      (* *)
      (__LOC__, {|(defn run [] (.hashCode (new String "2")))|}, {|50|});
      (__LOC__, {|(defn run [] (.hashCode (String. "2")))|}, {|50|});
      (* *)
      (__LOC__, {|(defn run [] (count [10 20 30]))|}, {|3|});
      (__LOC__, {|(defn run [] (count {:a 1 :b 2}))|}, {|2|});
      (__LOC__, {|(defn run [] (get [10 20 30] 1))|}, {|20|});
      (__LOC__, {|(defn run [] (get {:a 2 :b 3} :b))|}, {|3|});
      (__LOC__, {|(defn run [] (if (= (str "1" "2" "3") "123") 2 3))|}, {|2|});
      (__LOC__, {|(defn run [] (if (= (str 1 "2" 3) "123") 2 3))|}, {|2|});
      (__LOC__, {|(defn run [] (if (= 2 2) 3 4))|}, {|3|});
      (__LOC__, {|(defn run [] (if (instance? String "1") 2 3))|}, {|2|});
      (__LOC__, {|(defn run [] (if false 2 3))|}, {|3|});
      (__LOC__, {|(defn run [] (if true 2 3))|}, {|2|});
      (__LOC__, {|(defn run [] 2)|}, {|2|});
    ]
    |> JavaExecution.create_tests;
  ]
  |> List.concat
