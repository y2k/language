let assert_ code expected =
  let actual = Clj2js.main_kt "main.clj" code in
  if actual <> expected then (
    print_endline actual;
    print_newline ();
    failwith "actual <> expected")

let main () =
  assert_ {|(defn foo [^Int a ^Int b] (+ a b) (- a b))|}
    {|fun foo(a:Int, b:Int) : Any? = run { prelude.plus(a, b)
prelude.minus(a, b) };|};
  assert_ {|(defn foo [a b] (+ a b) (- a b))|}
    {|fun foo(a:Any?, b:Any?) : Any? = run { prelude.plus(a, b)
prelude.minus(a, b) };|};
  assert_ {|(defn foo [a b] (a b))|}
    {|fun foo(a:Any?, b:Any?) : Any? = run { a(b) };|};
  assert_ {|(defn foo [[a b]] (a b))|}
    {|fun foo(p__3:Any?) : Any? = run { val a = geta(p__3, 0); val b = geta(p__3, 1); a(b) };|};
  assert_ {|(defn foo [xs] (let [[a b] (foo 1 2)] (bar a b)))|}
    {|fun foo(xs:Any?) : Any? = run { val p__4 = foo(1, 2); val a = geta(p__4, 0); val b = geta(p__4, 1); bar(a, b) };|};
  assert_ {|(= a b)|} {|a == b|};
  assert_ {|(get xs 1)|} {|geta(xs, 1)|};
  assert_ {|(:foo bar)|} {|getm(bar, "foo")|};
  assert_ {|(def foo 1)|} {|val foo = 1;|};
  assert_ {|(def ^:private foo 1)|} {|private val foo = 1;|};
  assert_ {|(.map (listOf "") (fn [x] x))|} {|listOf("").map({ x -> x })|};
  assert_ {|(str 1 "2" 3)|} {|(""+1+"2"+3)|};
  assert_ {|(foo "a\"b")|} {|foo("a\"b")|};
  assert_ {|(__unsafe_inject_code "fun getm() = error(\"require Map\")")|}
    {|fun getm() = error("require Map")|};
  assert_ {|(__unsafe_inject_code "fun foo() = \"a\\\"b\"")|}
    {|fun foo() = "a\"b"|};
  ()
