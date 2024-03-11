let assert_ code expected =
  let actual = Lib.main_json "main.clj" code in
  if actual <> expected then (
    print_endline actual;
    print_newline ();
    failwith "actual <> expected")

let main () =
  assert_ "(Foo.)" {|["new","Foo"]|};
  assert_ "(Foo. a 1)" {|["new","Foo","a","1"]|};
  ()
