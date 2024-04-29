module Clj2js = Lib

let assert_ code expected =
  let prelude =
    In_channel.with_open_bin "../../../test/samples/prelude/js/src/prelude.clj"
      In_channel.input_all
  in
  let _ctx, actual = Clj2js.main_js "" "main.clj" prelude code in
  let start = 38 in
  let actual = String.sub actual start (String.length actual - start) in
  if actual <> expected then (
    print_endline actual;
    print_newline ();
    failwith "actual <> expected")

let assert_file filename =
  let path = "../../../test/samples/" ^ filename in
  let open In_channel in
  let code = with_open_bin path input_all in
  let expected = with_open_bin (path ^ ".js") input_all in
  assert_ code expected

let with_extenal_files files f =
  Lib__Linter.run_resolve
    (fun path ->
      match List.assoc_opt path files with
      | Some x -> x
      | None -> failwith @@ "file not found: " ^ path)
    f

let test1 () =
  assert_ {|(def LI_SP 600)|} {|export const LI_SP = 600;|};
  assert_ {|(def ^:private LI_SP 600)|} {|const LI_SP = 600;|};
  assert_ {|(def- LI_SP 600)|} {|const LI_SP = 600;|};

  with_extenal_files
    [ ("../vendor/effects/src/effects", "(def foo 1)") ]
    (fun _ ->
      assert_
        {|(ns app (:require ["../vendor/effects/src/effects" :as e])) (e/foo)|}
        {|import * as e from '../vendor/effects/src/effects.js';
e.foo()|});

  with_extenal_files
    [ ("a", "(def a 2)(def LI_SP 1)(def b 3)") ]
    (fun _ ->
      assert_ {|(ns _ (:require [a :as m])) m/LI_SP|}
        {|import * as m from './a.js';
m.LI_SP|});

  assert_ {|(Response. "hello_world" 1 false)|}
    {|new Response("hello_world", 1, false)|};
  assert_ "(let [r 0] (.json r))"
    "(function () { const r = 0; return r.json() })()";
  assert_ "(let [r 0 a 0] (.json r a))"
    "(function () { const r = 0; const a = 0; return r.json(a) })()";
  assert_ "(let [r 0 a 0 b 0] (.json r a b))"
    "(function () { const r = 0; const a = 0; const b = 0; return r.json(a, b) \
     })()";
  assert_
    {|(defn fetch [request env context]
                (.log console request)
                (.log console request))|}
    {|export const fetch = (request, env, context) => { console.log(request); return console.log(request) };|};
  assert_
    {|(defn fetch [request env context]
        (->
          'a 'b))|}
    "export const fetch = (request, env, context) => { return b(a) };";
  assert_
    {|(.next
       (.json 'request null)
       (fn [text] (println "???")))|}
    {|request.json(null).next((text) => { return console.info("???") })|};
  assert_
    {|(defn fetch [request env context]
        (->
         (.json request null)
         (.next (fn [text] (println "???")))))|}
    {|export const fetch = (request, env, context) => { return request.json(null).next((text) => { return console.info("???") }) };|};
  assert_
    {|(defn fetch [request env context]
        request)|}
    "export const fetch = (request, env, context) => { return request };";
  assert_ {|(export-default {:fetch 'fetch-handler})|}
    {|export default {"fetch": fetch-handler}|};
  assert_
    {|(defn fetch-handler [request env context] request)
     (export-default {:fetch fetch-handler})|}
    {|export const fetch-handler = (request, env, context) => { return request };
export default {"fetch": fetch-handler}|};
  assert_ "(println 1 2 3)" "console.info(1, 2, 3)";
  assert_ "('foo (println 1 2 3))" "foo(console.info(1, 2, 3))";
  assert_
    {|(comment 1 2 3)
     (println 1 2 3)
     (comment 1 2 3)|}
    "console.info(1, 2, 3)";
  assert_
    {|(export-default {:foo 1 :foo2 {:foo 1 :bar "2" :baz false} :bar "2" :baz false})|}
    {|export default {"foo": 1, "foo2": {"foo": 1, "bar": "2", "baz": false}, "bar": "2", "baz": false}|};
  assert_
    {|(if ('foo 1) 0 1)
   (if (if ('foo 'c0) 2 3)
     (if ('foo 'c1) 4 5)
     (if ('foo 'c2) 6 7))
     (if (= 1 2) 8 9)|}
    {|(foo(1) ? 0 : 1)
((foo(c0) ? 2 : 3) ? (foo(c1) ? 4 : 5) : (foo(c2) ? 6 : 7))
(1 === 2 ? 8 : 9)|};
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
    {|('map (fn [] 0) 'xs)
   ('map (fn [x] x) 'xs)|}
    {|map(() => { return 0 }, xs)
map((x) => { return x }, xs)|};
  assert_ "(and 'a (and 1 2 3) 'c 'd)" "(a && (1 && 2 && 3) && c && d)";
  assert_ "(< 'x 1)(> 'y 2)" "(x < 1)\n(y > 2)";
  assert_ "(<= 'x 1)(>= 'y 2)" "(x <= 1)\n(y >= 2)";
  assert_ "(def foo (+ 1 2))" "export const foo = (1 + 2);";
  assert_ "(let [b 1 a b?.c?.d?.e] a)"
    "(function () { const b = 1; const a = b?.c?.d?.e; return a })()";
  assert_ "(let [b 1 a b.c.d.e] a)"
    "(function () { const b = 1; const a = b.c.d.e; return a })()";
  assert_ "(or 'a 'b 1 2)" "(a || b || 1 || 2)";
  assert_ "(or 'a (fn [x] x) 1)" "(a || ((x) => { return x }) || 1)";
  assert_ "('foo 1)\n\n\n('foo 3)" "foo(1)\nfoo(3)";
  assert_ "('foo 1)\n;;('foo 2)\n('foo 3)" "foo(1)\nfoo(3)";
  assert_ "('foo 1)\n;;('foo 2.1)\n;;('foo 2.2)\n('foo 3)" "foo(1)\nfoo(3)";
  assert_ "('foo 1)\n\n;;('foo 2.1)\n;;('foo 2.2)\n\n('foo 3)" "foo(1)\nfoo(3)";
  assert_ "('foo 1)\n\n;;('foo 2.1)\n\n;;('foo 2.2)\n\n('foo 3)"
    "foo(1)\nfoo(3)";
  assert_ "(->> 0 ('a 1) ('b 2) ('c 3))" "c(3, b(2, a(1, 0)))";
  assert_ "(if-let [a 1 b 2 c 3] 'foo 'bar)"
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
  assert_ "(if-let* [a 1 b 2 c 3] 'foo 'bar)"
    "(function () { const a = 1; return (a ? (function () { const b = 2; \
     return (b ? (function () { const c = 3; return (c ? foo : bar) })() : \
     bar) })() : bar) })()";
  assert_ "(if-let* [a 1 b 2 c 3] (+ a b c) -1)"
    "(function () { const a = 1; return (a ? (function () { const b = 2; \
     return (b ? (function () { const c = 3; return (c ? (a + b + c) : -1) \
     })() : -1) })() : -1) })()";
  assert_ "(if-let* [_ 1 _ 2 _ 3] 6 -1)"
    "(function () { const _ = 1; return (_ ? (function () { const _ = 2; \
     return (_ ? (function () { const _ = 3; return (_ ? 6 : -1) })() : -1) \
     })() : -1) })()";
  assert_ {|('foo 1)(__unsafe_insert_js "import some")('bar 2)|}
    {|foo(1)
import some
bar(2)|};
  assert_ {|(assoc 'person :city "NY")|} {|{ ...person, city: "NY" }|};
  assert_ {|(assoc 'data.db 'user_id 'data.now)|}
    {|(function(){const temp={...data.db};temp[user_id]=data.now;return temp})()|};
  assert_ {|(-> ('foo 'person) (assoc :city "NY"))|}
    {|{ ...foo(person), city: "NY" }|};
  assert_ {|(-> 'person (assoc :city "NY"))|} {|{ ...person, city: "NY" }|};
  assert_ "(merge 'object1 'object2)" "{ ...object1, ...object2 }";
  assert_ "('sum (spread [1 2 3]))" "sum(...[1, 2, 3])";
  assert_ "(conj [1 2] 3)" "[...[1, 2], 3]";
  assert_ "(concat [1 2] [3 4])" "[...[1, 2], ...[3, 4]]";
  assert_ "[]" "[]";
  assert_ "{}" "{}";
  assert_ {|(throw (Error. "foo"))|} {|(function(){throw new Error("foo")})()|};
  assert_ "(not= 'a 'b)" "a !== b";
  assert_ "(/ 5 (/ 17 3))" "(5 / (17 / 3))";
  assert_ "(* 1 (* 2 3 4))" "(1 * (2 * 3 * 4))";
  assert_ "(defn- foo [x] x)" "const foo = (x) => { return x };";
  assert_ "(defn foo [x] x)" "export const foo = (x) => { return x };";
  assert_ "('foo (FIXME))"
    {|foo((function(){throw new Error(("" + "FIXME main.clj:1:7 - "))})())|};
  assert_ "('foo (FIXME 'A1 2))"
    {|foo((function(){throw new Error(("" + "FIXME main.clj:1:7 - " + A1 + 2))})())|};
  assert_ {|(println)|} {|console.info()|};
  assert_ {|(println "hello" 'world 123)|} {|console.info("hello", world, 123)|};
  (* assert_ "(ns a (:import [fs.promises fs]))" "import * as fs from 'fs/promises';"; *)
  assert_ "(ns a (:require [js.fs.promises :as fs]))"
    "import * as fs from 'fs/promises';";
  assert_ "(ns a (:require [vendor.effects :as e]))"
    "import * as e from './vendor.effects.js';";
  assert_ "(ns a (:require [vendor.effects :as e] [main :as app]))"
    "import * as e from './vendor.effects.js';\n\
     import * as app from './main.js';";
  assert_ {|(cond (= 1 2) 3 (= 4 5) 6 (= 7 8) 9 :else 0)|}
    {|(1 === 2 ? 3 : (4 === 5 ? 6 : (7 === 8 ? 9 : 0)))|};
  assert_ "(do ('foo 1 2) ('bar 3 4) ('baz 5 6))"
    "(function () { foo(1, 2); bar(3, 4); return baz(5, 6) })()";
  assert_ "(str 'a (if 'b 'c 'd))" {|("" + a + (b ? c : d))|};
  assert_ "(set! 'foo 1)" "(foo = 1);";
  assert_ "(set! 'foo.bar 1)" "(foo.bar = 1);";
  assert_ "(set! (.-bar 'foo) 1)" "(foo.bar = 1);";
  assert_ "(set! (.-bar ('foo 2)) 1)" "(foo(2).bar = 1);";
  assert_ "(set! (.-bar (get 'xs 2)) 1)" "(xs[2].bar = 1);";
  assert_ "(defmacro foo [a b] (list a 1)) (foo 'c d)" "c(1)";
  assert_ "(defmacro foo [a b] (list 'do (list a 1) (list b 2))) (foo 'c 'd)"
    "(function () { c(1); return d(2) })()";
  assert_ "(type 1)" "typeof 1";
  assert_ {|(= (type 'a) "String")|} {|typeof a === "String"|};
  assert_ "(not 'a)" "!(a)";
  assert_ "(not (+ 1 2))" "!((1 + 2))";
  assert_ "(get 'xs 7)" "xs[7]";
  assert_ "(case ('foo 1) 2 ('bar 22) ('getbaz 3) 'baz :qwe 3 'other)"
    {|(function () { const gen_1 = foo(1); return (gen_1 === 2 ? bar(22) : (gen_1 === getbaz(3) ? baz : (gen_1 === "qwe" ? 3 : other))) })()|};
  assert_ "(case 'key 2 ('bar 22) ('getbaz 3) 'baz :qwe 3 'other)"
    {|(function () { const gen_1 = key; return (gen_1 === 2 ? bar(22) : (gen_1 === getbaz(3) ? baz : (gen_1 === "qwe" ? 3 : other))) })()|};
  assert_ "(case 'key 2 ('bar 22) ('getbaz 3) 'baz :qwe 3 ('other 'a))"
    {|(function () { const gen_1 = key; return (gen_1 === 2 ? bar(22) : (gen_1 === getbaz(3) ? baz : (gen_1 === "qwe" ? 3 : other(a)))) })()|};
  assert_ "(fn [xs] (let [a (get xs 0) b (get xs 1)] (+ a b)))"
    "(xs) => { return (function () { const a = xs[0]; const b = xs[1]; return \
     (a + b) })() }";
  assert_ "(fn [[a b]] 0)"
    "(p__1) => { return (function () { const a = p__1[0]; const b = p__1[1]; \
     return 0 })() }";
  assert_ "(fn [c [a b] d] (+ a b c d))"
    "(c, p__1, d) => { return (function () { const a = p__1[0]; const b = \
     p__1[1]; return (a + b + c + d) })() }";
  assert_ "(ns app (:require [vendor.effects :as e] [main :as app]))"
    "import * as e from './vendor.effects.js';\n\
     import * as app from './main.js';";
  assert_ "(ns a (:require [js.fs.promises :as fs]))"
    "import * as fs from 'fs/promises';";
  assert_
    "(ns app (:require [vendor.effects :as e] [main :as app]) (:require \
     [js.fs.promises :as fs]))"
    "import * as e from './vendor.effects.js';\n\
     import * as app from './main.js';\n\
     import * as fs from 'fs/promises';";
  assert_ "(ns app (:require [vendor.effects :as e] [js.foo.wrangler :as fs]))"
    "import * as e from './vendor.effects.js';\n\
     import * as fs from 'foo/wrangler';";
  assert_ {|(assoc! 'data.db 7 'data.now)|} "data.db[7]=data.now";
  assert_ "[:div.tgme]" {|["div.tgme"]|};
  assert_ "{:div.tgme 'foo}" {|{"div.tgme": foo}|};
  assert_ "{:div 'foo}" {|{"div": foo}|};
  assert_ "(^export def foo (+ 1 2))" "export const foo = (1 + 2);";
  assert_ {|('foo "foo\"bar")|} {|foo("foo\"bar")|};
  assert_ "(% 1 2)" "(1 % 2)";
  assert_ {|('foo "a\"b")|} {|foo("a\"b")|};
  assert_ "(.play 'r)" "r.play()";
  assert_ "(. 'r play)" "r.play()";
  assert_ "(.-play 'r)" "r.play";
  assert_ "(. 'r -play)" "r.play";
  assert_ {|{:headers {:get (fn [] "TG_SECRET_TOKEN")}}|}
    {|{"headers": {"get": () => { return "TG_SECRET_TOKEN" }}}|};
  assert_ {|(cond (str "c") (str "a") :else (str "b"))|}
    {|(("" + "c") ? ("" + "a") : ("" + "b"))|};
  assert_ {|[(str "a")]|} {|[("" + "a")]|};
  assert_ "(('foo 'c 'd) 'a 'b)" "foo(c, d)(a, b)";
  assert_ {|(let [[a b] 'c] a)|}
    {|(function () { const p__1 = c; const a = p__1[0]; const b = p__1[1]; return a })()|};
  assert_ {|(:a 'b)|} {|b["a"]|};
  assert_ {|(jvm! (def a 1) (def b 2))|} {||};
  assert_ {|(js! (def a 1) (def b 2))|}
    "export const a = 1;\nexport const b = 2;";
  assert_ {|(fn [{a :url b :props}] [a b])|}
    {|(p__1) => { return (function () { const a = p__1["url"]; const b = p__1["props"]; return [a, b] })() }|};
  assert_ {|(atom 1)|} {|RT.atom(1)|};
  assert_ {|(deref 'x)|} {|RT.deref(x)|};
  assert_ {|(reset! 'x 2)|} {|RT.reset(x, 2)|};
  assert_ {|(swap! 'a (fn [x] x))|} {|RT.swap(a, (x) => { return x })|};
  assert_ {|(fn [a {b :b} c] (a b c))|}
    {|(a, p__1, c) => { return (function () { const b = p__1["b"]; return a(b, c) })() }|};
  assert_ {|(fn [a [b c] d] (a b c d))|}
    {|(a, p__1, d) => { return (function () { const b = p__1[0]; const c = p__1[1]; return a(b, c, d) })() }|};
  assert_ {|(defn a [b c & d] (println b c d))|}
    {|export const a = (b, c, ...d) => { return console.info(b, c, d) };|};
  assert_ {|(let [c 1 b c a b] a)|}
    {|(function () { const c = 1; const b = c; const a = b; return a })()|};
  assert_ {|(defn x [] (println))|}
    {|export const x = () => { return console.info() };|};
  assert_ {|(try 1 (catch :default e 2 3 e))|}
    {|(function() { try { return 1 } catch (e) { 2
3
return e } })()|};
  assert_ {|(defn foo [x] (foo x))|}
    {|export const foo = (x) => { return foo(x) };|};
  assert_ {|(comment (foo 1))(str 2)|} {|("" + 2)|};
  assert_ {|(jvm! (foo 1))(str 2)|} {|("" + 2)|};
  assert_ {|(ns resources (:require [main.shared :as app]))|}
    {|import * as app from './main.shared.js';|};
  ()

let test2 () =
  assert_
    "(defn fetch [^java.lang.Integer request ^kotlin.List env context] request)"
    "export const fetch = (request, env, context) => { return request };";
  assert_ "(defn foo [^an.app.Ac act ^an.we.WeVi webView] act)"
    "export const foo = (act, webView) => { return act };";
  assert_ "(defn foo [ ^an.app.Ac act ^an.we.WeVi webView] act)"
    "export const foo = (act, webView) => { return act };";
  assert_ {|(defn foo [^"(App)->aaa.Bbb" a ^"(Baz)->foo.Bar" b] a)|}
    "export const foo = (a, b) => { return a };";
  assert_ {|(defn foo [ ^"(App)->aaa.Bbb" a ^"(Baz)->foo.Bar" b] a)|}
    "export const foo = (a, b) => { return a };";
  assert_ "(defn foo [a b] (let [x (str 'e)] x))"
    {|export const foo = (a, b) => { return (function () { const x = ("" + e); return x })() };|};
  assert_ "(Foo.)" "new Foo()";
  assert_ "(Foo. 'a 1)" "new Foo(a, 1)";
  ()

let test3 () =
  assert_file "hotreload-client.clj";
  assert_file "sample1.clj";
  ()
