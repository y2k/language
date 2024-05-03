let () =
  (* Test_kt.main ();
  Test_ast.main (); *)
  Alcotest.run "Tests" (List.flatten [ Test_js.main (); Test_java.main () ])
