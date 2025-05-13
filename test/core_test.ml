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
      Printf.sprintf
        {|package y2k;

public class App {
%s;

public static void main(String[] args) {System.exit((int) RT.invoke(run));}}%s|}
        code prelude
    in
    prerr_endline code;
    let file_name = Filename.temp_file "y2k" ".java" in
    Out_channel.(with_open_bin file_name (fun f -> output_string f code));
    let result = Sys.command (Printf.sprintf "java %s" file_name) in
    string_of_int result

  let create_tests tests =
    tests
    |> List.map (fun (loc, i, e) ->
           Alcotest.test_case loc `Quick (fun () ->
               let code = Core.compile "user.clj" i in
               let a = run code in
               Alcotest.(check string) "" e a))
end

let tests =
  [
    (__LOC__, {|(ns _ (:import [java.util Date])) (defn run [] (.hashCode (Date. 2)))|}, {|999|});
    (* *)
    (__LOC__, {|(defn run [] (.hashCode (new String "2")))|}, {|50|});
    (__LOC__, {|(defn run [] (.hashCode (String. "2")))|}, {|50|});
    (* *)
    (__LOC__, {|(defn run [] (. "2" hashCode))|}, {|50|});
    (__LOC__, {|(defn run [] (.hashCode "2"))|}, {|50|});
    (__LOC__, {|(defn run [] (Integer/parseInt "2"))|}, {|2|});
    (* *)
    (__LOC__, {|(defn f [x] (if (= (str "1" x "3") "123") 2 3)) (defn run [] (f 2))|}, {|2|});
    (__LOC__, {|(defn f [x] x) (defn run [] (f 2))|}, {|2|});
    (__LOC__, {|(defn f [x] x) (defn run [] (f 3) (f 2))|}, {|2|});
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
