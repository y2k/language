let () =
  Alcotest.run "Tests"
    (List.flatten
       [ Test_js.main (); Test_java.main () (* Test_interp.main () *) ])
