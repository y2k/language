let () =
  Alcotest.run "Tests"
    [
      Test_file.tests;
      ( "Common (Eval)",
        Test_common.tests
        |> Test_eval.EvalExecution.create_tests `Quick "test/test_common.clj" );
      ( "Common (Java_v2)",
        Test_common.tests
        |> Test_java_v2.JavaV2Execution.create_tests `Quick
             ~namespace:"test_common" );
      ( "Common (JS)",
        Test_common.tests
        |> Test_js.create_test ~filename:"test_common.clj" `Quick );
      ("Sexp", Test_sexp.tests);
      ("Eval", Test_eval.tests);
      ("Java_v2", Test_java_v2.tests);
      ("JS", Test_js.tests);
      ("Regex", Test_regex.tests);
    ]
