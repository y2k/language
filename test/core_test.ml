(* let eval_tests =
  [
    (__LOC__, {|["x"]|}, {|"x"|});
    (__LOC__, "[1 2 3]", "123");
    (__LOC__, "[1 [2 3 4] 5]", "12345");
    (__LOC__, "(def* a 1) a", "1");
    (__LOC__, "(let* a 1) a", "1");
    (__LOC__, "(+ 1 2)", "3");
    (__LOC__, "1", "1");
    (__LOC__, {|"x"|}, {|"x"|});
    (__LOC__, "(str 1 2 3)", {|"123"|});
    (__LOC__, {|(str 1 "2" 3)|}, {|"123"|});
    (__LOC__, {|(str 1 (str 2 "3" 4) "5" 6)|}, {|"123456"|});
    (__LOC__, {|(def* f (fn* [x] x))(f 1)|}, "1");
    (__LOC__, {|(if* true 1 2)|}, "1");
    (__LOC__, {|(if* false 1 2)|}, "2");
  ]
  |> List.map (fun (loc, i, e) ->
         Alcotest.test_case loc `Quick (fun () ->
             let a = Core.eval i in
             Alcotest.(check string) "" e a)) *)

module JavaExecution : sig
  val run : string -> string
end = struct
  let run (code : string) =
    (* prerr_endline @@ "DIR: " ^ Sys.getcwd (); *)
    let prelude = In_channel.(with_open_bin "../../../y2k/RT.java" input_all) in
    let prelude = Str.global_replace (Str.regexp "package y2k;") "" prelude in
    let prelude = Str.global_replace (Str.regexp "public class RT") "class RT" prelude in
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
end

let java_integration_tests =
  [
    (__LOC__, {|(defn f [x] x) (defn run [] (f 2))|}, {|2|});
    (__LOC__, {|(defn f [x] x) (defn run [] (f 3) (f 2))|}, {|2|});
    (__LOC__, {|(defn run [] (count [10 20 30]))|}, {|3|});
    (__LOC__, {|(defn run [] (count {:a 1 :b 2}))|}, {|2|});
    (__LOC__, {|(defn run [] (get [10 20 30] 1))|}, {|20|});
    (__LOC__, {|(defn run [] (get {:a 2 :b 3} :b))|}, {|3|});
    (__LOC__, {|(defn run [] (if (= 2 2) 3 4))|}, {|3|});
    (__LOC__, {|(defn run [] (if (instance? String "1") 2 3))|}, {|2|});
    (__LOC__, {|(defn run [] (if false 2 3))|}, {|3|});
    (__LOC__, {|(defn run [] (if true 2 3))|}, {|2|});
    (__LOC__, {|(defn run [] 2)|}, {|2|});
  ]
  |> List.map (fun (loc, i, e) ->
         Alcotest.test_case loc `Quick (fun () ->
             let code = Core.compile "user.clj" i in
             let a = JavaExecution.run code in
             Alcotest.(check string) "" e a))

(* let java_tests =
  [ (__LOC__, "(str 1 2 3)", "String.format(\"%s%s%s\",1,2,3)");
    (__LOC__, {|(if* true 1 2)|}, "if (true) {\n1\n} else {\n2\n}");
    (__LOC__, {|(def* f (fn* [x] x)) (f 1)|}, {|public static Object f=y2k.RT.fn((x)->{
return x;
});
y2k.RT.invoke(f,1)|});
    (__LOC__, {|(def* a 1)|}, "public static Object a=1");
    (__LOC__, {|(def* f (fn* [x] x))|}, {|public static Object f=y2k.RT.fn((x)->{
return x;
})|});
    (__LOC__, {|[1 2 3]|}, "java.util.Arrays.asList(1,2,3)");
    (__LOC__, {|["x"]|}, {|java.util.Arrays.asList("x")|});
    (__LOC__, {|[1 [1 2 3] 3]|}, "java.util.Arrays.asList(1,java.util.Arrays.asList(1,2,3),3)"); ]
  |> List.map (fun (loc, i, e) ->
         Alcotest.test_case loc `Quick (fun () ->
             let a = Core.compile "user.clj" i in
             Alcotest.(check string) "" e a)) *)

let tests =
  (* java_tests @ *)
  (* eval_tests @  *)
  java_integration_tests
