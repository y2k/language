let assert_ code expected =
  let actual = Lib.main_json "main.clj" code in
  if actual <> expected then (
    print_endline actual;
    print_newline ();
    failwith "actual <> expected")

let main () =
  assert_ "(Foo.)" {|["new","Foo"]|};
  assert_ "(Foo. a 1)" {|["new","Foo","a","1"]|};
  assert_ {|(defmacro a [& xs] (list 'b (vec xs))) (a 1 2 3)|}
    {|["b",["vector","1","2","3"]]|};
  assert_ {|(let [foo (fn [a b] a)](let [bar (fn [a] (foo a a))]))|}
    {|["let*",["foo",["fn*",["a","b"],"a"]],["let*",["bar",["fn*",["a"],["foo","a","a"]]]]]|};
  assert_ {|(defn foo [a b] a)(defn bar [a] (foo a a))|}
    {|["let*",["foo",["fn*",["a","b"],"a"]],["let*",["bar",["fn*",["a"],["foo","a","a"]]],[]]]|};
  ()
