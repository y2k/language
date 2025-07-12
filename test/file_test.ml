module F = Core__.Common.Files

let assets_ loc path expected =
  Alcotest.test_case loc `Slow (fun () ->
      let actual = F.realpath ~root:"/app/src" path in
      Alcotest.(check string) "" expected actual)

let tests =
  ( "Files",
    [
      assets_ __LOC__ "../baz/../foo/bar.txt" "/app/foo/bar.txt";
      assets_ __LOC__ "../../foo/bar.txt" "/foo/bar.txt";
      assets_ __LOC__ "../foo/bar.txt" "/app/foo/bar.txt";
      assets_ __LOC__ "./foo/bar.txt" "/app/src/foo/bar.txt";
      assets_ __LOC__ "foo/bar.txt" "/app/src/foo/bar.txt";
    ] )
