let assert_ code expected =
  let actual = Clj2js.main code in
  if actual <> expected then failwith actual

let () =
  assert_
    {|(defn fetch [request env context]
   (.log console request)
   (.log console request))|}
    {|const fetch = (request, env, context) => { console.log(request); return console.log(request) }|};
  assert_
    {|(defn fetch [request env context]
     (->
       a b))|}
    "const fetch = (request, env, context) => { return b(a) }";
  assert_
    {|(defn fetch [request env context]
     (->
      (.json request null)
      (.next (fn [text] (failwith "???")))))|}
    {|const fetch = (request, env, context) => { return request.json(null).next((text) => { return failwith("???") }) }|};
  assert_
    {|(defn fetch [request env context]
     request)|}
    "const fetch = (request, env, context) => { return request }";
  assert_
    {|(defn fetch-handler [request env context] request)
  (export-default {:fetch fetch-handler})|}
    {|const fetch-handler = (request, env, context) => { return request }
export default { fetch: fetch-handler }|};
  assert_ {|(Response. "hello_world" 1 false)|}
    {|new Response("hello_world", 1, false)|};
  assert_ {|(comment 1 2 3)
  (println 1 2 3)
  (comment 1 2 3)|}
    "\nprintln(1, 2, 3)\n";
  assert_
    {|(export-default {:foo 1 :foo2 {:foo 1 :bar "2" :baz false} :bar "2" :baz false})|}
    {|export default { foo: 1, foo2: { foo: 1, bar: "2", baz: false }, bar: "2", baz: false }|};
  assert_
    {|(if (foo 1) a b)
(if (if (foo c0) a0 b0)
  (if (foo c1) a1 b1)
  (if (foo c2) a2 b2))
  (if (= 1 2) a b)|}
    {|(foo(1)) ? (a) : (b)
((foo(c0)) ? (a0) : (b0)) ? ((foo(c1)) ? (a1) : (b1)) : ((foo(c2)) ? (a2) : (b2))
(1 == 2) ? (a) : (b)|};
  assert_ "(+ 1 (+ 10 20 30) 3 (str 1) 5)"
    "(1 + (10 + 20 + 30) + 3 + str(1) + 5)";
  assert_ "(- 1 (- 10 20))" "(1 - (10 - 20))";
  assert_ "(- 1 2 3 4)" "(1 - 2 - 3 - 4)";
  assert_ {|{"content-type" "application/json" :a [1 [10 20 30] 3]}|}
    {|{ "content-type": "application/json", a: [1, [10, 20, 30], 3] }|};
  assert_ {|(println "hello world")|} {|println("hello world")|};
  assert_
    {|(println)
(println )
(println 1)
(Response. )
(Response.)
(Response. 1)|}
    {|println()
println()
println(1)
new Response()
new Response()
new Response(1)|};
  assert_ {|(map (fn [] 0) xs)
(map (fn [x] x) xs)|}
    {|map(() => { return 0 }, xs)
map((x) => { return x }, xs)|};
  assert_ "(and a (and 1 2 3) c d)" "(a && (1 && 2 && 3) && c && d)";
  assert_ "(< x 1)(> y 2)" "(x < 1)\n(y > 2)";
  assert_ "(<= x 1)(>= y 2)" "(x <= 1)\n(y >= 2)";
  assert_ "(def foo (+ 1 2))" "const foo = (1 + 2);";
  assert_ "(let [a b?.c?.d?.e] a)"
    "(function () { const a = b?.c?.d?.e; return a })()";
  assert_ "(let [a b.c.d.e] a)"
    "(function () { const a = b.c.d.e; return a })()";
  assert_ "(.json r)" "r.json()";
  assert_ "(.json r a)" "r.json(a)";
  assert_ "(.json r a b)" "r.json(a, b)";
  assert_ "(or a b 1 2)" "(a || b || 1 || 2)";
  ()