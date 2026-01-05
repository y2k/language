module F = Core__.Common.Files

let test_realpath pos path expected =
  Alcotest.test_case path `Slow (fun () ->
      let actual = F.realpath ~root:"/app/src" path in
      Alcotest.(check ?pos:(Some pos) string) "" expected actual)

let tests =
  ( "Files",
    [
      test_realpath __POS__ "../baz/../foo/bar.txt" "/app/foo/bar.txt";
      test_realpath __POS__ "../../foo/bar.txt" "/foo/bar.txt";
      test_realpath __POS__ "../foo/bar.txt" "/app/foo/bar.txt";
      test_realpath __POS__ "./foo/bar.txt" "/app/src/foo/bar.txt";
      test_realpath __POS__ "foo/bar.txt" "/app/src/foo/bar.txt";
    ] )
