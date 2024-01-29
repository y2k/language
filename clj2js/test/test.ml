let assert_ code expected =
  let actual = Clj2js.main "main.clj" code in
  if actual <> expected then (
    print_endline actual;
    print_newline ();
    failwith "actual <> expected")

let assert_file filename =
  let read_sample filename =
    let channel = open_in ("../../../test/samples/" ^ filename) in
    let size = in_channel_length channel in
    let content = really_input_string channel size in
    close_in channel;
    content
  in
  let code = read_sample filename in
  let expected = read_sample (filename ^ ".js") in
  assert_ code expected

let () =
  assert_
    {|(defn fetch [request env context]
                (.log console request)
                (.log console request))|}
    {|export const fetch = (request, env, context) => { console.log(request); return console.log(request) }|};
  assert_
    {|(defn fetch [request env context]
        (->
          a b))|}
    "export const fetch = (request, env, context) => { return b(a) }";
  assert_
    {|(defn fetch [request env context]
        (->
         (.json request null)
         (.next (fn [text] (failwith "???")))))|}
    {|export const fetch = (request, env, context) => { return request.json(null).next((text) => { return failwith("???") }) }|};
  assert_
    {|(defn fetch [request env context]
        request)|}
    "export const fetch = (request, env, context) => { return request }";
  assert_
    {|(defn fetch-handler [request env context] request)
     (export-default {:fetch fetch-handler})|}
    {|export const fetch-handler = (request, env, context) => { return request }
export default {"fetch": fetch-handler}|};
  assert_ {|(Response. "hello_world" 1 false)|}
    {|new Response("hello_world", 1, false)|};
  assert_
    {|(comment 1 2 3)
     (println 1 2 3)
     (comment 1 2 3)|}
    "console.info(1, 2, 3)";
  assert_
    {|(export-default {:foo 1 :foo2 {:foo 1 :bar "2" :baz false} :bar "2" :baz false})|}
    {|export default {"foo": 1, "foo2": {"foo": 1, "bar": "2", "baz": false}, "bar": "2", "baz": false}|};
  assert_
    {|(if (foo 1) a b)
   (if (if (foo c0) a0 b0)
     (if (foo c1) a1 b1)
     (if (foo c2) a2 b2))
     (if (= 1 2) a b)|}
    {|(foo(1) ? a : b)
((foo(c0) ? a0 : b0) ? (foo(c1) ? a1 : b1) : (foo(c2) ? a2 : b2))
(1 === 2 ? a : b)|};
  assert_ "(+ 1 (+ 10 20 30) 3 (str 1) 5)"
    {|(1 + (10 + 20 + 30) + 3 + ("" + 1) + 5)|};
  assert_ "(- 1 (- 10 20))" "(1 - (10 - 20))";
  assert_ "(- 1 2 3 4)" "(1 - 2 - 3 - 4)";
  assert_ {|{"content-type" "application/json" :a [1 [10 20 30] 3]}|}
    {|{"content-type": "application/json", "a": [1, [10, 20, 30], 3]}|};
  assert_ {|(println "hello world")|} {|console.info("hello world")|};
  assert_
    {|(println)
   (println )
   (println 1)
   (Response. )
   (Response.)
   (Response. 1)|}
    {|console.info()
console.info()
console.info(1)
new Response()
new Response()
new Response(1)|};
  assert_
    {|(map (fn [] 0) xs)
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
  assert_ "(foo 1)\n\n\n(foo 3)" "foo(1)\nfoo(3)";
  assert_ "(foo 1)\n;;(foo 2)\n(foo 3)" "foo(1)\nfoo(3)";
  assert_ "(foo 1)\n;;(foo 2.1)\n;;(foo 2.2)\n(foo 3)" "foo(1)\nfoo(3)";
  assert_ "(foo 1)\n\n;;(foo 2.1)\n;;(foo 2.2)\n\n(foo 3)" "foo(1)\nfoo(3)";
  assert_ "(foo 1)\n\n;;(foo 2.1)\n\n;;(foo 2.2)\n\n(foo 3)" "foo(1)\nfoo(3)";
  assert_ "(->> 0 (a 1) (b 2) (c 3))" "c(3, b(2, a(1, 0)))";
  assert_ "(if-let [a 1 b 2 c 3] foo bar)"
    "(function () { const a = 1; return (a ? (function () { const b = 2; \
     return (b ? (function () { const c = 3; return (c ? foo : bar) })() : \
     bar) })() : bar) })()";
  assert_ "(if-let [a 1 b 2 c 3] (+ a b c) -1)"
    "(function () { const a = 1; return (a ? (function () { const b = 2; \
     return (b ? (function () { const c = 3; return (c ? (a + b + c) : -1) \
     })() : -1) })() : -1) })()";
  assert_ "(if-let [_ 1 _ 2 _ 3] 6 -1)"
    "(function () { const _ = 1; return (_ ? (function () { const _ = 2; \
     return (_ ? (function () { const _ = 3; return (_ ? 6 : -1) })() : -1) \
     })() : -1) })()";
  assert_ {|(foo 1)(__unsafe_insert_js "import some")(bar 2)|}
    {|foo(1)
import some
bar(2)|};
  assert_ {|(assoc person :city "NY")|} {|{ ...person, city: "NY" }|};
  assert_ {|(-> (foo person) (assoc :city "NY"))|}
    {|{ ...foo(person), city: "NY" }|};
  assert_ {|(-> person (assoc :city "NY"))|} {|{ ...person, city: "NY" }|};
  assert_ "(merge object1 object2)" "{ ...object1, ...object2 }";
  assert_ "(sum (spread [1 2 3]))" "sum(...[1, 2, 3])";
  assert_ "(conj [1 2] 3)" "[...[1, 2], 3]";
  assert_ "(concat [1 2] [3 4])" "[...[1, 2], ...[3, 4]]";
  assert_ "[]" "[]";
  assert_ "{}" "{}";
  assert_ {|(throw (Error. "foo"))|} {|(function(){throw new Error("foo")})()|};
  assert_ "(not= a b)" "a !== b";
  assert_ "(/ 5 (/ 17 3))" "(5 / (17 / 3))";
  assert_ "(* 1 (* 2 3 4))" "(1 * (2 * 3 * 4))";
  assert_ "(defn- foo [x] x)" "const foo = (x) => { return x };";
  assert_ "(defn foo [x] x)" "export const foo = (x) => { return x }";
  assert_ "(foo FIXME)"
    {|foo((function(){throw new Error("Not implemented main.clj:1:6")})())|};
  assert_ "(foo (FIXME A1 B2))"
    {|foo((function(){throw new Error(("" + "FIXME main.clj:1:6 - " + A1 + B2))})())|};
  assert_ {|(println)|} {|console.info()|};
  assert_ {|(println "hello" world 123)|} {|console.info("hello", world, 123)|};
  assert_ "(import [fs.promises :as fs])" "import * as fs from 'fs/promises';";
  assert_ "(require [vendor.effects :as e] [main :as app])"
    "import * as e from './vendor/effects.js';\n\
     import * as app from './main.js';";
  assert_ {|(cond (= 1 2) 3 (= 4 5) 6 (= 7 8) 9 :else 0)|}
    {|(1 === 2 ? 3 : (4 === 5 ? 6 : (7 === 8 ? 9 : 0)))|};
  assert_ "(do (foo 1 2) (bar 3 4) (baz 5 6))"
    "(function () { foo(1, 2); bar(3, 4); return baz(5, 6) })()";
  assert_ "(str a (if b c d))" {|("" + a + (b ? c : d))|};
  assert_ {|(map (λ [] 0) xs)
(map (λ [x] x) xs)|}
    {|map(() => { return 0 }, xs)
map((x) => { return x }, xs)|};
  ()

let () =
  assert_file "hotreload-client.clj";
  assert_file "sample1.clj";
  ()
