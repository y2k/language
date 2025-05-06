let tests =
  [
    (*  *)
    (*  *)
    (__LOC__, "[1 [1 2 3] 3]", "java.util.Arrays.asList(1, java.util.Arrays.asList(1, 2, 3), 3)");
    (__LOC__, "1", "1");
    (__LOC__, {|["x"]|}, {|java.util.Arrays.asList("x")|});
    (__LOC__, {|"x"|}, {|"x"|});
    (__LOC__, "(+ 1 2)", "3");
    (__LOC__, "[1 2 3]", "java.util.Arrays.asList(1, 2, 3)");
    (__LOC__, "(def a 1)", "public static Object a=1;");
  ]
  (* |> List.map (fun (loc, i, e) -> (loc, e, Core.compile i)) *)
  |> List.map (fun (loc, e, i) ->
         Alcotest.test_case loc `Quick (fun () ->
             let a = Core.compile i in
             Alcotest.(check string) "" e a))
