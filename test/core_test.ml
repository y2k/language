let eval_tests =
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
             Alcotest.(check string) "" e a))

let java_tests =
  [
    (__LOC__, "(str 1 2 3)", "String.format(\"%s%s%s\",1,2,3)");
    (__LOC__, {|(if* true 1 2)|}, "if (true) {\n1\n} else {\n2\n}");
    (__LOC__, {|(def* f (fn* [x] x)) (f 1)|}, {|public static Object f=y2k.RT.fn((x)->{x});
y2k.RT.invoke(f,1)|});
    (__LOC__, {|(def* a 1)|}, "public static Object a=1");
    (__LOC__, {|(def* f (fn* [x] x))|}, {|public static Object f=y2k.RT.fn((x)->{x})|});
    (__LOC__, {|[1 2 3]|}, "java.util.Arrays.asList(1,2,3)");
    (__LOC__, {|["x"]|}, {|java.util.Arrays.asList("x")|});
    (__LOC__, {|[1 [1 2 3] 3]|}, "java.util.Arrays.asList(1,java.util.Arrays.asList(1,2,3),3)");
  ]
  |> List.map (fun (loc, i, e) ->
         Alcotest.test_case loc `Quick (fun () ->
             let a = Core.compile i in
             Alcotest.(check string) "" e a))

let tests = eval_tests @ java_tests
