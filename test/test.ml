let () =
  Alcotest.run "Tests"
    [
      (* *)
      (* *)
      ("Java", Java_test.tests);
      Js_test.tests;
      File_test.tests;
    ]
