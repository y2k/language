module F = Core__.Common.Files

let test_realpath loc path expected =
  Alcotest.test_case loc `Slow (fun () ->
      let actual = F.realpath ~root:"/app/src" path in
      Alcotest.(check string) "" expected actual)

let tests =
  ( "Files",
    [
      test_realpath __LOC__ "../baz/../foo/bar.txt" "/app/foo/bar.txt";
      test_realpath __LOC__ "../../foo/bar.txt" "/foo/bar.txt";
      test_realpath __LOC__ "../foo/bar.txt" "/app/foo/bar.txt";
      test_realpath __LOC__ "./foo/bar.txt" "/app/src/foo/bar.txt";
      test_realpath __LOC__ "foo/bar.txt" "/app/src/foo/bar.txt";
    ] )
