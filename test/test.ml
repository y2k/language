let () =
  Alcotest.run "Tests"
    [
      File_test.tests;
      (* *)
      (* ("Eval", Eval_test.tests);
      ("Java", Java_test.tests);
      ("JS", Js_test.tests); *)
      (* *)
      ( "Common (Eval)",
        Test_common.tests |> Eval_test.EvalExecution.create_tests );
      ( "Common (Java)",
        Test_common.tests |> Java_test.JavaExecution.create_tests );
      ("Common (JS)", Test_common.tests |> Js_test.create_test);
    ]
