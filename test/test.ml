let () =
  Alcotest.run "Tests"
    [
      ("Eval", Eval_test.tests);
      ("Java", Java_test.tests);
      ("JS", Js_test.tests);
      File_test.tests;
    ]
