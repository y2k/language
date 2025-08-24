let () =
  Alcotest.run ~bail:true "Tests"
    [
      File_test.tests;
      (* *)
      ( "Common (Eval)",
        Test_common.tests
        |> Eval_test.EvalExecution.create_tests `Quick "test/test_common.clj" );
      ( "Common (Java)",
        Test_common.tests
        |> Java_test.JavaExecution.create_tests `Quick "test/test_common.clj"
             "test_common" );
      ("Common (JS)", Test_common.tests |> Js_test.create_test `Quick);
      (* *)
      ("Sexp", Test_sexp.tests);
      ("Eval", Eval_test.tests);
      ("Java", Java_test.tests);
      ("JS", Js_test.tests);
      (* *)
    ]
