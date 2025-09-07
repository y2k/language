module E = Core__.Functions_eval

let tests =
  [
    (__POS__, "\\d+", "abc12345def", Some [ "12345" ]);
    (__POS__, "^\\d+", "abc12345def", None);
    (__POS__, "^\\d+", "678abc12345def", Some [ "678" ]);
    (__POS__, "abc(123)", "678abc12345def", Some [ "abc123"; "123" ]);
    (__POS__, "^\\d+", "12bb", Some [ "12" ]);
  ]
  |> List.map (fun (pos, pattern, input, expected) ->
         Alcotest.test_case (Printf.sprintf "'%s' '%s'" pattern input) `Quick
           (fun () ->
             let actual = E.re_find pattern input in
             Alcotest.(check ?pos:(Some pos) (option (list string)))
               "" expected actual))
