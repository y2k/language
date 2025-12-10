let () =
  Alcotest.run "Tests"
    [
      Test_file.tests;
      (* *)
      ( "Common (Eval)",
        Test_common.tests_old
        |> Test_eval.EvalExecution.create_tests `Quick "test/test_common.clj" );
      ( "Common (Java)",
        Test_common.tests
        |> Test_java.JavaExecution.create_tests `Quick ~namespace:"test_common"
      );
      ( "Common (JS)",
        Test_common.tests_old
        |> Test_js.create_test ~filename:"test_common.clj" `Quick );
      (* *)
      ("Sexp", Test_sexp.tests);
      ("Eval", Test_eval.tests);
      ("Java", Test_java.tests);
      ("JS", Test_js.tests);
      ("Regex", Test_regex.tests);
      (* *)
    ]
