let () =
  Alcotest.run "Tests"
    [
      (* *)
      (* *)
      ("V2", Java_test.tests);
      Js_test.tests;
      File_test.tests;
    ]
