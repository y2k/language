module Clj2js = Lib

let assert1 code expected =
  let inner_assert () =
    let prelude =
      In_channel.with_open_bin
        "../../../test/samples/prelude/js/src/prelude.clj" In_channel.input_all
    in
    let _ctx, actual =
      Lib__.Frontend.NameGenerator.with_scope (fun _ ->
          Clj2js.main_js "main.clj" prelude code)
    in
    let start = 15 in
    let actual = String.sub actual start (String.length actual - start) in
    Alcotest.(check string) "1" expected actual
  in
  Alcotest.test_case "assert_" `Quick inner_assert

let assert2 files code expected =
  let with_extenal_files files f =
    Lib__Linter.run_resolve
      (fun path ->
        match List.assoc_opt path files with
        | Some x -> x
        | None -> failwith @@ "file not found: " ^ path)
      f
  in
  let inner_assert () =
    let prelude =
      In_channel.with_open_bin
        "../../../test/samples/prelude/js/src/prelude.clj" In_channel.input_all
    in
    let _ctx, actual =
      with_extenal_files files (fun () ->
          Lib__.Frontend.NameGenerator.with_scope (fun _ ->
              Clj2js.main_js "main.clj" prelude code))
    in
    let start = 15 in
    let actual = String.sub actual start (String.length actual - start) in
    Alcotest.(check string) "1" expected actual
  in
  Alcotest.test_case "assert_" `Quick inner_assert

let assert_file filename =
  let path = "../../../test/samples/" ^ filename in
  let open In_channel in
  let code = with_open_bin path input_all in
  let expected = with_open_bin (path ^ ".js") input_all in
  assert1 code expected

let test1 =
  [
    assert1 {|(def LI_SP 600)|} {|export const LI_SP = 600;|};
    assert1 {|(def ^:private LI_SP 600)|} {|const LI_SP = 600;|};
    assert1 {|(def- LI_SP 600)|} {|const LI_SP = 600;|};
    assert2
      [ ("../vendor/effects/src/effects", "(def foo 1)") ]
      {|(ns app (:require ["../vendor/effects/src/effects" :as e])) (e/foo)|}
      {|import * as e from '../vendor/effects/src/effects.js';
e.foo()|};
    assert2
      [ ("a", "(def a 2)(def LI_SP 1)(def b 3)") ]
      {|(ns _ (:require [a :as m])) m/LI_SP|}
      {|import * as m from './a.js';
m.LI_SP|};
    assert1 {|(Response. "hello_world" 1 false)|}
      {|new Response("hello_world", 1, false)|};
    assert1 "(let [r 0] (.json r))"
      "(function () { const r = 0; return r.json() })()";
    assert1 "(let [r 0 a 0] (.json r a))"
      "(function () { const r = 0; const a = 0; return r.json(a) })()";
    assert1 "(let [r 0 a 0 b 0] (.json r a b))"
      "(function () { const r = 0; const a = 0; const b = 0; return r.json(a, \
       b) })()";
    assert1
      {|(defn fetch [request env context]
                (.log console request)
                (.log console request))|}
      {|export const fetch = (request, env, context) => { console.log(request); return console.log(request) };|};
    assert1
      {|(defn fetch [request env context]
        (->
          'a 'b))|}
      "export const fetch = (request, env, context) => { return b(a) };";
    assert1
      {|(.next
       (.json 'request null)
       (fn [text] (println "???")))|}
      {|request.json(null).next((text) => { return console.info("???") })|};
    assert1
      {|(defn fetch [request env context]
        (->
         (.json request null)
         (.next (fn [text] (println "???")))))|}
      {|export const fetch = (request, env, context) => { return request.json(null).next((text) => { return console.info("???") }) };|};
    assert1
      {|(defn fetch [request env context]
        request)|}
      "export const fetch = (request, env, context) => { return request };";
    assert1 {|(export-default {:fetch 'fetch-handler})|}
      {|export default {"fetch": fetch-handler}|};
    assert1
      {|(defn fetch-handler [request env context] request)
     (export-default {:fetch fetch-handler})|}
      {|export const fetch-handler = (request, env, context) => { return request };
export default {"fetch": fetch-handler}|};
    assert1 "(println 1 2 3)" "console.info(1, 2, 3)";
    assert1 "('foo (println 1 2 3))" "foo(console.info(1, 2, 3))";
    assert1
      {|(comment 1 2 3)
     (println 1 2 3)
     (comment 1 2 3)|}
      "console.info(1, 2, 3)";
    assert1
      {|(export-default {:foo 1 :foo2 {:foo 1 :bar "2" :baz false} :bar "2" :baz false})|}
      {|export default {"foo": 1, "foo2": {"foo": 1, "bar": "2", "baz": false}, "bar": "2", "baz": false}|};
    assert1
      {|(if ('foo 1) 0 1)
   (if (if ('foo 'c0) 2 3)
     (if ('foo 'c1) 4 5)
     (if ('foo 'c2) 6 7))
     (if (= 1 2) 8 9)|}
      {|(foo(1) ? 0 : 1)
((foo(c0) ? 2 : 3) ? (foo(c1) ? 4 : 5) : (foo(c2) ? 6 : 7))
(1 === 2 ? 8 : 9)|};
    assert1 "(+ 1 (+ 10 20 30) 3 (str 1) 5)"
      {|(1 + (10 + 20 + 30) + 3 + ("" + 1) + 5)|};
    assert1 "(- 1 (- 10 20))" "(1 - (10 - 20))";
    assert1 "(- 1 2 3 4)" "(1 - 2 - 3 - 4)";
    assert1 {|{"content-type" "application/json" :a [1 [10 20 30] 3]}|}
      {|{"content-type": "application/json", "a": [1, [10, 20, 30], 3]}|};
    assert1 {|(println "hello world")|} {|console.info("hello world")|};
    assert1
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
    assert1
      {|('map (fn [] 0) 'xs)
   ('map (fn [x] x) 'xs)|}
      {|map(() => { return 0 }, xs)
map((x) => { return x }, xs)|};
    assert1 "(and 'a (and 1 2 3) 'c 'd)" "(a && (1 && 2 && 3) && c && d)";
    assert1 "(< 'x 1)(> 'y 2)" "(x < 1)\n(y > 2)";
    assert1 "(<= 'x 1)(>= 'y 2)" "(x <= 1)\n(y >= 2)";
    assert1 "(def foo (+ 1 2))" "export const foo = (1 + 2);";
    assert1 "(let [b 1 a b?.c?.d?.e] a)"
      "(function () { const b = 1; const a = b?.c?.d?.e; return a })()";
    assert1 "(let [b 1 a b.c.d.e] a)"
      "(function () { const b = 1; const a = b.c.d.e; return a })()";
    assert1 "(or 'a 'b 1 2)" "(a || b || 1 || 2)";
    assert1 "(or 'a (fn [x] x) 1)" "(a || ((x) => { return x }) || 1)";
    assert1 "('foo 1)\n\n\n('foo 3)" "foo(1)\nfoo(3)";
    assert1 "('foo 1)\n;;('foo 2)\n('foo 3)" "foo(1)\nfoo(3)";
    assert1 "('foo 1)\n;;('foo 2.1)\n;;('foo 2.2)\n('foo 3)" "foo(1)\nfoo(3)";
    assert1 "('foo 1)\n\n;;('foo 2.1)\n;;('foo 2.2)\n\n('foo 3)"
      "foo(1)\nfoo(3)";
    assert1 "('foo 1)\n\n;;('foo 2.1)\n\n;;('foo 2.2)\n\n('foo 3)"
      "foo(1)\nfoo(3)";
    assert1 "(->> 0 ('a 1) ('b 2) ('c 3))" "c(3, b(2, a(1, 0)))";
    assert1 "(if-let [a 1 b 2 c 3] 'foo 'bar)"
      "(function () { const a = 1; return (a ? (function () { const b = 2; \
       return (b ? (function () { const c = 3; return (c ? foo : bar) })() : \
       bar) })() : bar) })()";
    assert1 "(if-let [a 1 b 2 c 3] (+ a b c) -1)"
      "(function () { const a = 1; return (a ? (function () { const b = 2; \
       return (b ? (function () { const c = 3; return (c ? (a + b + c) : -1) \
       })() : -1) })() : -1) })()";
    assert1 "(if-let [_ 1 _ 2 _ 3] 6 -1)"
      "(function () { const _ = 1; return (_ ? (function () { const _ = 2; \
       return (_ ? (function () { const _ = 3; return (_ ? 6 : -1) })() : -1) \
       })() : -1) })()";
    assert1 "(if-let* [a 1 b 2 c 3] 'foo 'bar)"
      "(function () { const a = 1; return (a ? (function () { const b = 2; \
       return (b ? (function () { const c = 3; return (c ? foo : bar) })() : \
       bar) })() : bar) })()";
    assert1 "(if-let* [a 1 b 2 c 3] (+ a b c) -1)"
      "(function () { const a = 1; return (a ? (function () { const b = 2; \
       return (b ? (function () { const c = 3; return (c ? (a + b + c) : -1) \
       })() : -1) })() : -1) })()";
    assert1 "(if-let* [_ 1 _ 2 _ 3] 6 -1)"
      "(function () { const _ = 1; return (_ ? (function () { const _ = 2; \
       return (_ ? (function () { const _ = 3; return (_ ? 6 : -1) })() : -1) \
       })() : -1) })()";
    assert1 {|('foo 1)(__unsafe_insert_js "import some")('bar 2)|}
      {|foo(1)
import some
bar(2)|};
    assert1 {|(assoc 'person :city "NY")|} {|{ ...person, city: "NY" }|};
    assert1 {|(assoc 'data.db 'user_id 'data.now)|}
      {|(function(){const temp={...data.db};temp[user_id]=data.now;return temp})()|};
    assert1 {|(-> ('foo 'person) (assoc :city "NY"))|}
      {|{ ...foo(person), city: "NY" }|};
    assert1 {|(-> 'person (assoc :city "NY"))|} {|{ ...person, city: "NY" }|};
    assert1 "(merge 'object1 'object2)" "{ ...object1, ...object2 }";
    assert1 "('sum (spread [1 2 3]))" "sum(...[1, 2, 3])";
    assert1 "(conj [1 2] 3)" "[...[1, 2], 3]";
    assert1 "(concat [1 2] [3 4])" "[...[1, 2], ...[3, 4]]";
    assert1 "[]" "[]";
    assert1 "{}" "{}";
    assert1 {|(throw (Error. "foo"))|}
      {|(function(){throw new Error("foo")})()|};
    assert1 "(not= 'a 'b)" "a !== b";
    assert1 "(/ 5 (/ 17 3))" "(5 / (17 / 3))";
    assert1 "(* 1 (* 2 3 4))" "(1 * (2 * 3 * 4))";
    assert1 "(defn- foo [x] x)" "const foo = (x) => { return x };";
    assert1 "(defn foo [x] x)" "export const foo = (x) => { return x };";
    assert1 "('foo (FIXME))"
      {|foo((function(){throw new Error(("" + "FIXME main.clj:1:7 - "))})())|};
    assert1 "('foo (FIXME 'A1 2))"
      {|foo((function(){throw new Error(("" + "FIXME main.clj:1:7 - " + A1 + 2))})())|};
    assert1 {|(println)|} {|console.info()|};
    assert1 {|(println "hello" 'world 123)|}
      {|console.info("hello", world, 123)|};
    (* assert_ "(ns a (:import [fs.promises fs]))" "import * as fs from 'fs/promises';"; *)
    assert1 "(ns a (:require [js.fs.promises :as fs]))"
      "import * as fs from 'fs/promises';";
    assert1 "(ns a (:require [vendor.effects :as e]))"
      "import * as e from './vendor.effects.js';";
    assert1 "(ns a (:require [vendor.effects :as e] [main :as app]))"
      "import * as e from './vendor.effects.js';\n\
       import * as app from './main.js';";
    assert1 {|(cond (= 1 2) 3 (= 4 5) 6 (= 7 8) 9 :else 0)|}
      {|(1 === 2 ? 3 : (4 === 5 ? 6 : (7 === 8 ? 9 : 0)))|};
    assert1 "(do ('foo 1 2) ('bar 3 4) ('baz 5 6))"
      "(function () { foo(1, 2); bar(3, 4); return baz(5, 6) })()";
    assert1 "(str 'a (if 'b 'c 'd))" {|("" + a + (b ? c : d))|};
    assert1 "(set! 'foo 1)" "(foo = 1);";
    assert1 "(set! 'foo.bar 1)" "(foo.bar = 1);";
    assert1 "(set! (.-bar 'foo) 1)" "(foo.bar = 1);";
    assert1 "(set! (.-bar ('foo 2)) 1)" "(foo(2).bar = 1);";
    assert1 "(set! (.-bar (get 'xs 2)) 1)" "(xs[2].bar = 1);";
    assert1 "(defmacro foo [a b] (list a 1)) (foo 'c d)" "c(1)";
    assert1 "(defmacro foo [a b] (list 'do (list a 1) (list b 2))) (foo 'c 'd)"
      "(function () { c(1); return d(2) })()";
    assert1 "(type 1)" "typeof 1";
    assert1 {|(= (type 'a) "String")|} {|typeof a === "String"|};
    assert1 "(not 'a)" "!(a)";
    assert1 "(not (+ 1 2))" "!((1 + 2))";
    assert1 "(get 'xs 7)" "xs[7]";
    assert1 "(case ('foo 1) 2 ('bar 22) ('getbaz 3) 'baz :qwe 3 'other)"
      {|(function () { const gen_1 = foo(1); return (gen_1 === 2 ? bar(22) : (gen_1 === getbaz(3) ? baz : (gen_1 === "qwe" ? 3 : other))) })()|};
    assert1 "(case 'key 2 ('bar 22) ('getbaz 3) 'baz :qwe 3 'other)"
      {|(function () { const gen_1 = key; return (gen_1 === 2 ? bar(22) : (gen_1 === getbaz(3) ? baz : (gen_1 === "qwe" ? 3 : other))) })()|};
    assert1 "(case 'key 2 ('bar 22) ('getbaz 3) 'baz :qwe 3 ('other 'a))"
      {|(function () { const gen_1 = key; return (gen_1 === 2 ? bar(22) : (gen_1 === getbaz(3) ? baz : (gen_1 === "qwe" ? 3 : other(a)))) })()|};
    assert1 "(fn [xs] (let [a (get xs 0) b (get xs 1)] (+ a b)))"
      "(xs) => { return (function () { const a = xs[0]; const b = xs[1]; \
       return (a + b) })() }";
    assert1 "(fn [[a b]] 0)"
      "(p__1) => { return (function () { const a = p__1[0]; const b = p__1[1]; \
       return 0 })() }";
    assert1 "(fn [c [a b] d] (+ a b c d))"
      "(c, p__1, d) => { return (function () { const a = p__1[0]; const b = \
       p__1[1]; return (a + b + c + d) })() }";
    assert1 "(ns app (:require [vendor.effects :as e] [main :as app]))"
      "import * as e from './vendor.effects.js';\n\
       import * as app from './main.js';";
    assert1 "(ns a (:require [js.fs.promises :as fs]))"
      "import * as fs from 'fs/promises';";
    assert1
      "(ns app (:require [vendor.effects :as e] [main :as app]) (:require \
       [js.fs.promises :as fs]))"
      "import * as e from './vendor.effects.js';\n\
       import * as app from './main.js';\n\
       import * as fs from 'fs/promises';";
    assert1
      "(ns app (:require [vendor.effects :as e] [js.foo.wrangler :as fs]))"
      "import * as e from './vendor.effects.js';\n\
       import * as fs from 'foo/wrangler';";
    assert1 {|(assoc! 'data.db 7 'data.now)|} "data.db[7]=data.now";
    assert1 "[:div.tgme]" {|["div.tgme"]|};
    assert1 "{:div.tgme 'foo}" {|{"div.tgme": foo}|};
    assert1 "{:div 'foo}" {|{"div": foo}|};
    assert1 "(^export def foo (+ 1 2))" "export const foo = (1 + 2);";
    assert1 {|('foo "foo\"bar")|} {|foo("foo\"bar")|};
    assert1 "(% 1 2)" "(1 % 2)";
    assert1 {|('foo "a\"b")|} {|foo("a\"b")|};
    assert1 "(.play 'r)" "r.play()";
    assert1 "(. 'r play)" "r.play()";
    assert1 "(.-play 'r)" "r.play";
    assert1 "(. 'r -play)" "r.play";
    assert1 {|{:headers {:get (fn [] "TG_SECRET_TOKEN")}}|}
      {|{"headers": {"get": () => { return "TG_SECRET_TOKEN" }}}|};
    assert1 {|(cond (str "c") (str "a") :else (str "b"))|}
      {|(("" + "c") ? ("" + "a") : ("" + "b"))|};
    assert1 {|[(str "a")]|} {|[("" + "a")]|};
    assert1 "(('foo 'c 'd) 'a 'b)" "foo(c, d)(a, b)";
    assert1 {|(let [[a b] 'c] a)|}
      {|(function () { const p__1 = c; const a = p__1[0]; const b = p__1[1]; return a })()|};
    assert1 {|(:a 'b)|} {|b["a"]|};
    assert1 {|(jvm! (def a 1) (def b 2))|} {||};
    assert1 {|(js! (def a 1) (def b 2))|}
      "export const a = 1;\nexport const b = 2;";
    assert1 {|(fn [{a :url b :props}] [a b])|}
      {|(p__1) => { return (function () { const a = p__1["url"]; const b = p__1["props"]; return [a, b] })() }|};
    assert1 {|(atom 1)|} {|Array.of(1)|};
    assert1 {|(deref 'x)|} {|x[0]|};
    assert1 {|(reset! 'x 2)|} {|(function () { x.fill(2); return 2 })()|};
    assert1 {|(swap! 'a (fn [x] x))|}
      {|a.splice(0, 1, ((x) => { return x })(a[0]))[0]|};
    assert1 {|(fn [a {b :b} c] (a b c))|}
      {|(a, p__1, c) => { return (function () { const b = p__1["b"]; return a(b, c) })() }|};
    assert1 {|(fn [a [b c] d] (a b c d))|}
      {|(a, p__1, d) => { return (function () { const b = p__1[0]; const c = p__1[1]; return a(b, c, d) })() }|};
    assert1 {|(defn a [b c & d] (println b c d))|}
      {|export const a = (b, c, ...d) => { return console.info(b, c, d) };|};
    assert1 {|(let [c 1 b c a b] a)|}
      {|(function () { const c = 1; const b = c; const a = b; return a })()|};
    assert1 {|(defn x [] (println))|}
      {|export const x = () => { return console.info() };|};
    assert1 {|(try 1 (catch :default e 2 3 e))|}
      {|(function() { try { return 1 } catch (e) { 2
3
return e } })()|};
    assert1 {|(defn foo [x] (foo x))|}
      {|export const foo = (x) => { return foo(x) };|};
    assert1 {|(comment (foo 1))(str 2)|} {|("" + 2)|};
    assert1 {|(jvm! (foo 1))(str 2)|} {|("" + 2)|};
    assert1 {|(ns resources (:require [main.shared :as app]))|}
      {|import * as app from './main.shared.js';|};
  ]

let test2 =
  [
    assert1
      "(defn fetch [^java.lang.Integer request ^kotlin.List env context] \
       request)"
      "export const fetch = (request, env, context) => { return request };";
    assert1 "(defn foo [^an.app.Ac act ^an.we.WeVi webView] act)"
      "export const foo = (act, webView) => { return act };";
    assert1 "(defn foo [ ^an.app.Ac act ^an.we.WeVi webView] act)"
      "export const foo = (act, webView) => { return act };";
    assert1 {|(defn foo [^"(App)->aaa.Bbb" a ^"(Baz)->foo.Bar" b] a)|}
      "export const foo = (a, b) => { return a };";
    assert1 {|(defn foo [ ^"(App)->aaa.Bbb" a ^"(Baz)->foo.Bar" b] a)|}
      "export const foo = (a, b) => { return a };";
    assert1 "(defn foo [a b] (let [x (str 'e)] x))"
      {|export const foo = (a, b) => { return (function () { const x = ("" + e); return x })() };|};
    assert1 "(Foo.)" "new Foo()";
    assert1 "(Foo. 'a 1)" "new Foo(a, 1)";
  ]

let main () =
  Alcotest.run "Utils"
    [
      ("test1", test1);
      ("test2", test2);
      ( "files",
        [
          assert_file "hotreload-client.clj";
          assert_file "sample1.clj";
          assert_file "main.shared.clj";
        ] );
    ]
