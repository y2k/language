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
                 {|(ns _) (defn foo [x] x)|} |> Core.compile false "/app/src/core/ext/lib/eff.clj" "/app/src"
               in
               let code = Core.compile true "/app/src/core/ext/user.clj" "/app/src" input in
               let actual = ext_module ^ "\n" ^ code |> run in
               Alcotest.(check string) "" expected actual))
end

(* let re_find pattern str =
  let pattern = pattern |> Str.global_replace (Str.regexp "(") "\\(" |> Str.global_replace (Str.regexp ")") "\\)" in
  let re = Str.regexp pattern in
  try
    let _result = Str.search_forward re str 0 in
    (* prerr_endline @@ "LOG: " ^ string_of_int _result; *)

    let groups =
      Seq.unfold
        (fun i ->
          try
            let r = Str.matched_group i str in
            Some (r, i + 1)
          with Invalid_argument _ -> None)
        1
      |> List.of_seq
    in
    Some (Str.matched_string str :: groups)
  with Not_found -> None

let () =
  [
    (Some [ "abcdxxx"; "bcdxx" ], re_find "a([a-z]+)x" "xabcdxxx");
    (Some [ "abcdxxx"; "bcdxx" ], re_find "a(.+)x" "xabcdxxx");
    (Some [ "abcdx" ], re_find "abcdx" "xabcdxxx");
    (Some [ "abcdx" ], re_find "abcdx" "xabcdxxx");
    (Some [ "abcd"; "bc" ], re_find "a(bc)d" "xabcd");
    (Some [ "abcd"; "bc" ], re_find "a(bc)d" "xabcdxxx");
    (Some [ "abcdx"; "bc"; "x" ], re_find "a(bc)d(x)" "xabcdxxx");
    (Some [ "abcdxxx"; "bc"; "xxx" ], re_find "a(bc)d(x+)" "xabcdxxx");
    (None, re_find "a(bc)d" "");
  ]
  |> List.map (fun (e, a) ->
         Alcotest.test_case "Java" `Quick (fun () -> Alcotest.(check (option (list string))) "." e a))
  |> fun x -> Alcotest.run "core" [ ("Java", x) ] *)

let tests =
  [
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
    (__LOC__, {|(ns _ (:require ["./lib/eff" :as e])) (defn run [] (e/foo 42))|}, {|42|});
    (__LOC__, {|(ns _ (:import [java.util Date])) (defn run [] (.hashCode (Date. 42)))|}, {|42|});
    (* *)
    (__LOC__, {|(defn run [] ((fn [x] (+ x x)) 21))|}, {|42|});
    (* *)
    (__LOC__, {|(defn run [] (.hashCode (new String "2")))|}, {|50|});
    (__LOC__, {|(defn run [] (.hashCode (String. "2")))|}, {|50|});
    (* *)
    (__LOC__, {|(defn run [] (. "2" hashCode))|}, {|50|});
    (__LOC__, {|(defn run [] (.hashCode "2"))|}, {|50|});
    (__LOC__, {|(defn run [] (Integer/parseInt "2"))|}, {|2|});
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
  |> JavaExecution.create_tests
