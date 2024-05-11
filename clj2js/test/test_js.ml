let assert_ = Utils.assert_ Lib.main_js "js/src/prelude.clj"

let assert_with_import =
  Utils.assert_with_import Lib.main_js "js/src/prelude.clj"

let assert_file = Utils.assert_file Lib.main_js "js/src/prelude.clj" ".js"

let test1 =
  [
    assert_ __POS__ {|(def LI_SP 600)|} {|export const LI_SP = 600;|};
    assert_ __POS__ {|(def ^:private LI_SP 600)|} {|const LI_SP = 600;|};
    assert_ __POS__ {|(def- LI_SP 600)|} {|const LI_SP = 600;|};
    assert_with_import __POS__
      [ ("../vendor/effects/src/effects", "(def foo 1)") ]
      {|(ns app (:require ["../vendor/effects/src/effects" :as e])) (e/foo)|}
      {|import * as e from '../vendor/effects/src/effects.js';
e.foo()|};
    assert_with_import __POS__
      [ ("a", "(def a 2)(def LI_SP 1)(def b 3)") ]
      {|(ns _ (:require [a :as m])) m/LI_SP|}
      {|import * as m from './a.js';
m.LI_SP|};
    assert_ __POS__ {|(Response. "hello_world" 1 false)|}
      {|new Response("hello_world", 1, false)|};
    assert_ __POS__ "(let [r 0] (.json r))"
      "(function () { const r = 0; return r.json() })()";
    assert_ __POS__ "(let [r 0 a 0] (.json r a))"
      "(function () { const r = 0; const a = 0; return r.json(a) })()";
    assert_ __POS__ "(let [r 0 a 0 b 0] (.json r a b))"
      "(function () { const r = 0; const a = 0; const b = 0; return r.json(a, \
       b) })()";
    assert_ __POS__
      {|(defn fetch [request env context]
                (.log console request)
                (.log console request))|}
      {|export const fetch = ((request, env, context) => { console.log(request); return console.log(request) });|};
    assert_ __POS__
      {|(defn fetch [request env context]
        (->
          'a 'b))|}
      "export const fetch = ((request, env, context) => { return b(a) });";
    assert_ __POS__
      {|(.next
       (.json 'request null)
       (fn [text] (println "hello")))|}
      {|request.json(null).next(((text) => { return console.info("hello") }))|};
    assert_ __POS__
      {|(defn fetch [request env context]
        (->
         (.json request null)
         (.next (fn [text] (println "hello")))))|}
      {|export const fetch = ((request, env, context) => { return request.json(null).next(((text) => { return console.info("hello") })) });|};
    assert_ __POS__
      {|(defn fetch [request env context]
        request)|}
      "export const fetch = ((request, env, context) => { return request });";
    assert_ __POS__ {|(export-default {:fetch 'fetch_handler})|}
      {|export default {"fetch": fetch_handler}|};
    assert_ __POS__ {|(export-default {:fetch fetch_export})|}
      {|export default {"fetch": fetch_export}|};
    assert_ __POS__
      {|(defn fetch-handler [request env context] request)
     (export-default {:fetch fetch-handler})|}
      {|export const fetch-handler = ((request, env, context) => { return request });
export default {"fetch": fetch-handler}|};
    assert_ __POS__ "(println 1 2 3)" "console.info(1, 2, 3)";
    assert_ __POS__ "('foo (println 1 2 3))" "foo(console.info(1, 2, 3))";
    assert_ __POS__
      {|(comment 1 2 3)
     (println 1 2 3)
     (comment 1 2 3)|}
      "console.info(1, 2, 3)";
    assert_ __POS__
      {|(export-default {:foo 1 :foo2 {:foo 1 :bar "2" :baz false} :bar "2" :baz false})|}
      {|export default {"foo": 1, "foo2": {"foo": 1, "bar": "2", "baz": false}, "bar": "2", "baz": false}|};
    assert_ __POS__
      {|(if ('foo 1) 0 1)
   (if (if ('foo 'c0) 2 3)
     (if ('foo 'c1) 4 5)
     (if ('foo 'c2) 6 7))
     (if (= 1 2) 8 9)|}
      {|(foo(1) ? 0 : 1)
((foo(c0) ? 2 : 3) ? (foo(c1) ? 4 : 5) : (foo(c2) ? 6 : 7))
(1 === 2 ? 8 : 9)|};
    assert_ __POS__ "(+ 1 (+ 10 20 30) 3 (str 1) 5)"
      {|(1 + (10 + 20 + 30) + 3 + ("" + 1) + 5)|};
    assert_ __POS__ "(- 1 (- 10 20))" "(1 - (10 - 20))";
    assert_ __POS__ "(- 1 2 3 4)" "(1 - 2 - 3 - 4)";
    assert_ __POS__ {|{"content-type" "application/json" :a [1 [10 20 30] 3]}|}
      {|{"content-type": "application/json", "a": [1, [10, 20, 30], 3]}|};
    assert_ __POS__ {|(println "hello world")|} {|console.info("hello world")|};
    assert_ __POS__
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
    assert_ __POS__
      {|('map (fn [] 0) 'xs)
   ('map (fn [x] x) 'xs)|}
      {|map((() => { return 0 }), xs)
map(((x) => { return x }), xs)|};
    assert_ __POS__ "(and 1)" "(1)";
    assert_ __POS__ "(and 1 2)" "(1 && 2)";
    assert_ __POS__ "(and 'a (and 1 2 3) 'c 'd)"
      "(a && (1 && 2 && 3) && c && d)";
    assert_ __POS__ "(< 'x 1)(> 'y 2)" "(x < 1)\n(y > 2)";
    assert_ __POS__ "(<= 'x 1)(>= 'y 2)" "(x <= 1)\n(y >= 2)";
    assert_ __POS__ "(def foo (+ 1 2))" "export const foo = (1 + 2);";
    assert_ __POS__ "(let [b 1 a b?.c?.d?.e] a)"
      "(function () { const b = 1; const a = b?.c?.d?.e; return a })()";
    assert_ __POS__ "(let [b 1 a b.c.d.e] a)"
      "(function () { const b = 1; const a = b.c.d.e; return a })()";
    assert_ __POS__ "(or 1)" "(1)";
    assert_ __POS__ "(or 'a 'b 1 2)" "(a || b || 1 || 2)";
    assert_ __POS__ "(or 'a (fn [x] x) 1)" "(a || ((x) => { return x }) || 1)";
    assert_ __POS__ "('foo 1)\n\n\n('foo 3)" "foo(1)\nfoo(3)";
    assert_ __POS__ "('foo 1)\n;;('foo 2)\n('foo 3)" "foo(1)\nfoo(3)";
    assert_ __POS__ "('foo 1)\n;;('foo 2.1)\n;;('foo 2.2)\n('foo 3)"
      "foo(1)\nfoo(3)";
    assert_ __POS__ "('foo 1)\n\n;;('foo 2.1)\n;;('foo 2.2)\n\n('foo 3)"
      "foo(1)\nfoo(3)";
    assert_ __POS__ "('foo 1)\n\n;;('foo 2.1)\n\n;;('foo 2.2)\n\n('foo 3)"
      "foo(1)\nfoo(3)";
    assert_ __POS__ "(->> 0 ('a 1) ('b 2) ('c 3))" "c(3, b(2, a(1, 0)))";
    assert_ __POS__ "(if-let [a 1 b 2 c 3] 'foo 'bar)"
      "(function () { const a = 1; return (a ? (function () { const b = 2; \
       return (b ? (function () { const c = 3; return (c ? foo : bar) })() : \
       bar) })() : bar) })()";
    assert_ __POS__ "(if-let [a 1 b 2 c 3] (+ a b c) -1)"
      "(function () { const a = 1; return (a ? (function () { const b = 2; \
       return (b ? (function () { const c = 3; return (c ? (a + b + c) : -1) \
       })() : -1) })() : -1) })()";
    assert_ __POS__ "(if-let [_ 1 _ 2 _ 3] 6 -1)"
      "(function () { const _ = 1; return (_ ? (function () { const _ = 2; \
       return (_ ? (function () { const _ = 3; return (_ ? 6 : -1) })() : -1) \
       })() : -1) })()";
    assert_ __POS__ "(if-let* [a 1 b 2 c 3] 'foo 'bar)"
      "(function () { const a = 1; return (a ? (function () { const b = 2; \
       return (b ? (function () { const c = 3; return (c ? foo : bar) })() : \
       bar) })() : bar) })()";
    assert_ __POS__ "(if-let* [a 1 b 2 c 3] (+ a b c) -1)"
      "(function () { const a = 1; return (a ? (function () { const b = 2; \
       return (b ? (function () { const c = 3; return (c ? (a + b + c) : -1) \
       })() : -1) })() : -1) })()";
    assert_ __POS__ "(if-let* [_ 1 _ 2 _ 3] 6 -1)"
      "(function () { const _ = 1; return (_ ? (function () { const _ = 2; \
       return (_ ? (function () { const _ = 3; return (_ ? 6 : -1) })() : -1) \
       })() : -1) })()";
    assert_ __POS__ {|(assoc 'person :city "NY")|} {|{ ...person, city: "NY" }|};
    assert_ __POS__ {|(assoc 'data.db 'user_id 'data.now)|}
      {|(function(){const temp={...data.db};temp[user_id]=data.now;return temp})()|};
    assert_ __POS__ {|(-> ('foo 'person) (assoc :city "NY"))|}
      {|{ ...foo(person), city: "NY" }|};
    assert_ __POS__ {|(-> 'person (assoc :city "NY"))|}
      {|{ ...person, city: "NY" }|};
    assert_ __POS__ "(merge 'object1 'object2)" "{ ...object1, ...object2 }";
    assert_ __POS__ "('sum (spread [1 2 3]))" "sum(...[1, 2, 3])";
    assert_ __POS__ "(conj [1 2] 3)" "[...[1, 2], 3]";
    assert_ __POS__ "(concat [1 2] [3 4])" "[...[1, 2], ...[3, 4]]";
    assert_ __POS__ "[]" "[]";
    assert_ __POS__ "{}" "{}";
    assert_ __POS__ {|(throw (Error. "foo"))|}
      {|(function(){throw new Error("foo")})()|};
    assert_ __POS__ "(not= 'a 'b)" "!(a === b)";
    assert_ __POS__ "(/ 5 (/ 17 3))" "(5 / (17 / 3))";
    assert_ __POS__ "(* 1 (* 2 3 4))" "(1 * (2 * 3 * 4))";
    assert_ __POS__ "(defn- foo [x] x)" "const foo = ((x) => { return x });";
    assert_ __POS__ "(defn foo [x] x)"
      "export const foo = ((x) => { return x });";
    assert_ __POS__ "('foo (FIXME))"
      {|foo((function(){throw new Error(("" + "FIXME main.clj:1:7 - "))})())|};
    assert_ __POS__ "('foo (FIXME 'A1 2))"
      {|foo((function(){throw new Error(("" + "FIXME main.clj:1:7 - " + A1 + 2))})())|};
    assert_ __POS__ {|(println)|} {|console.info()|};
    assert_ __POS__ {|(println "hello" 'world 123)|}
      {|console.info("hello", world, 123)|};
    (* assert_ "(ns a (:import [fs.promises fs]))" "import * as fs from 'fs/promises';"; *)
    assert_ __POS__ "(ns a (:require [js.fs.promises :as fs]))"
      "import * as fs from 'fs/promises';";
    assert_ __POS__ "(ns a (:require [vendor.effects :as e]))"
      "import * as e from './vendor.effects.js';";
    assert_ __POS__ "(ns a (:require [vendor.effects :as e] [main :as app]))"
      "import * as e from './vendor.effects.js';\n\
       import * as app from './main.js';";
    assert_ __POS__ {|(cond (= 1 2) 3 (= 4 5) 6 (= 7 8) 9 :else 0)|}
      {|(1 === 2 ? 3 : (4 === 5 ? 6 : (7 === 8 ? 9 : 0)))|};
    assert_ __POS__ "(do ('foo 1 2) ('bar 3 4) ('baz 5 6))"
      "(function () { foo(1, 2); bar(3, 4); return baz(5, 6) })()";
    assert_ __POS__ "(str 'a (if 'b 'c 'd))" {|("" + a + (b ? c : d))|};
    assert_ __POS__ "(set! 'foo 1)" "(foo = 1);";
    assert_ __POS__ "(set! 'foo.bar 1)" "(foo.bar = 1);";
    assert_ __POS__ "(set! (.-bar 'foo) 1)" "(foo.bar = 1);";
    assert_ __POS__ "(set! (.-bar ('foo 2)) 1)" "(foo(2).bar = 1);";
    assert_ __POS__ "(set! (.-bar (get 'xs 2)) 1)" "(xs[2].bar = 1);";
    assert_ __POS__ "(defmacro foo [a b] (list a 1)) (foo 'c d)" "c(1)";
    assert_ __POS__
      "(defmacro foo [a b] (list 'do (list a 1) (list b 2))) (foo 'c 'd)"
      "(function () { c(1); return d(2) })()";
    assert_ __POS__ "(type 1)" "typeof 1";
    assert_ __POS__ {|(= (type 'a) "String")|} {|typeof a === "String"|};
    assert_ __POS__ "(not 'a)" "!(a)";
    assert_ __POS__ "(not (+ 1 2))" "!((1 + 2))";
    assert_ __POS__ "(get 'xs 7)" "xs[7]";
    assert_ __POS__ "(case ('foo 1) 2 ('bar 22) ('getbaz 3) 'baz :qwe 3 'other)"
      {|(function () { const gen_1 = foo(1); return (gen_1 === 2 ? bar(22) : (gen_1 === getbaz(3) ? baz : (gen_1 === "qwe" ? 3 : other))) })()|};
    assert_ __POS__ "(case 'key 2 ('bar 22) ('getbaz 3) 'baz :qwe 3 'other)"
      {|(function () { const gen_1 = key; return (gen_1 === 2 ? bar(22) : (gen_1 === getbaz(3) ? baz : (gen_1 === "qwe" ? 3 : other))) })()|};
    assert_ __POS__
      "(case 'key 2 ('bar 22) ('getbaz 3) 'baz :qwe 3 ('other 'a))"
      {|(function () { const gen_1 = key; return (gen_1 === 2 ? bar(22) : (gen_1 === getbaz(3) ? baz : (gen_1 === "qwe" ? 3 : other(a)))) })()|};
    assert_ __POS__ "(fn [xs] (let [a (get xs 0) b (get xs 1)] (+ a b)))"
      "((xs) => { return (function () { const a = xs[0]; const b = xs[1]; \
       return (a + b) })() })";
    assert_ __POS__ "(fn [[a b]] 0)"
      "((p__1) => { return (function () { const a = p__1[0]; const b = \
       p__1[1]; return 0 })() })";
    assert_ __POS__ "(fn [c [a b] d] (+ a b c d))"
      "((c, p__1, d) => { return (function () { const a = p__1[0]; const b = \
       p__1[1]; return (a + b + c + d) })() })";
    assert_ __POS__ "(ns app (:require [vendor.effects :as e] [main :as app]))"
      "import * as e from './vendor.effects.js';\n\
       import * as app from './main.js';";
    assert_ __POS__ "(ns a (:require [js.fs.promises :as fs]))"
      "import * as fs from 'fs/promises';";
    assert_ __POS__
      "(ns app (:require [vendor.effects :as e] [main :as app]) (:require \
       [js.fs.promises :as fs]))"
      "import * as e from './vendor.effects.js';\n\
       import * as app from './main.js';\n\
       import * as fs from 'fs/promises';";
    assert_ __POS__
      "(ns app (:require [vendor.effects :as e] [js.foo.wrangler :as fs]))"
      "import * as e from './vendor.effects.js';\n\
       import * as fs from 'foo/wrangler';";
    assert_ __POS__ {|(assoc! 'data.db 7 'data.now)|} "data.db[7]=data.now";
    assert_ __POS__ "[:div.tgme]" {|["div.tgme"]|};
    assert_ __POS__ "{:div.tgme 'foo}" {|{"div.tgme": foo}|};
    assert_ __POS__ "{:div 'foo}" {|{"div": foo}|};
    assert_ __POS__ "(^export def foo (+ 1 2))" "export const foo = (1 + 2);";
    assert_ __POS__ {|('foo "foo\"bar")|} {|foo("foo\"bar")|};
    assert_ __POS__ "(% 1 2)" "(1 % 2)";
    assert_ __POS__ {|('foo "a\"b")|} {|foo("a\"b")|};
    assert_ __POS__ "(.play 'r)" "r.play()";
    assert_ __POS__ "(. 'r play)" "r.play()";
    assert_ __POS__ "(.-play 'r)" "r.play";
    assert_ __POS__ "(. 'r -play)" "r.play";
    assert_ __POS__ {|{:headers {:get (fn [] "TG_SECRET_TOKEN")}}|}
      {|{"headers": {"get": (() => { return "TG_SECRET_TOKEN" })}}|};
    assert_ __POS__ {|(cond (str "c") (str "a") :else (str "b"))|}
      {|(("" + "c") ? ("" + "a") : ("" + "b"))|};
    assert_ __POS__ {|[(str "a")]|} {|[("" + "a")]|};
    assert_ __POS__ "(('foo 'c 'd) 'a 'b)" "foo(c, d)(a, b)";
    assert_ __POS__ {|(let [[a b] 'c] a)|}
      {|(function () { const p__1 = c; const a = p__1[0]; const b = p__1[1]; return a })()|};
    assert_ __POS__ {|(:a 'b)|} {|b["a"]|};
    assert_ __POS__ {|(jvm! (def a 1) (def b 2))|} {||};
    assert_ __POS__ {|(js! (def a 1) (def b 2))|}
      "export const a = 1;\nexport const b = 2;";
    assert_ __POS__ {|(fn [{a :url b :props}] [a b])|}
      {|((p__1) => { return (function () { const a = p__1["url"]; const b = p__1["props"]; return [a, b] })() })|};
    assert_ __POS__ {|(atom 1)|} {|Array.of(1)|};
    assert_ __POS__ {|(deref 'x)|} {|x[0]|};
    assert_ __POS__ {|(reset! 'x 2)|}
      {|(function () { x.fill(2); return 2 })()|};
    assert_ __POS__ {|(swap! 'a (fn [x] x))|}
      {|a.splice(0, 1, (((x) => { return x }))(a[0]))[0]|};
    assert_ __POS__ {|(fn [a {b :b} c] (a b c))|}
      {|((a, p__1, c) => { return (function () { const b = p__1["b"]; return a(b, c) })() })|};
    assert_ __POS__ {|(fn [a [b c] d] (a b c d))|}
      {|((a, p__1, d) => { return (function () { const b = p__1[0]; const c = p__1[1]; return a(b, c, d) })() })|};
    assert_ __POS__ {|(defn a [b c & d] (println b c d))|}
      {|export const a = ((b, c, ...d) => { return console.info(b, c, d) });|};
    assert_ __POS__ {|(let [c 1 b c a b] a)|}
      {|(function () { const c = 1; const b = c; const a = b; return a })()|};
    assert_ __POS__ {|(defn x [] (println))|}
      {|export const x = (() => { return console.info() });|};
    assert_ __POS__ {|(try 1 (catch :default e 2 3 e))|}
      {|(function() { try { return 1 } catch (e) { 2
3
return e } })()|};
    assert_ __POS__ {|(defn foo [x] (foo x))|}
      {|export const foo = ((x) => { return foo(x) });|};
    assert_ __POS__ {|(comment (foo 1))(str 2)|} {|("" + 2)|};
    assert_ __POS__ {|(jvm! (foo 1))(str 2)|} {|("" + 2)|};
    assert_ __POS__ {|(ns resources (:require [main.shared :as app]))|}
      {|import * as app from './main.shared.js';|};
    assert_ __POS__ {|(.join 'r)|} {|r.join()|};
    assert_ __POS__ {|[(.join 'r)]|} {|[r.join()]|};
    assert_ __POS__ {|{:b (.join 'r)}|} {|{"b": r.join()}|};
    assert_ __POS__
      {|(defn- tr [user] (let [un "c"] (str "a" un "b" user.id ")")))|}
      {|const tr = ((user) => { return (function () { const un = "c"; return ("" + "a" + un + "b" + user.id + ")") })() });|};
    assert_ __POS__
      {|(defn- tr [user] {:cp (let [un "c"] (str "a" un "b" user.id ")"))})|}
      {|const tr = ((user) => { return {"cp": (function () { const un = "c"; return ("" + "a" + un + "b" + user.id + ")") })()} });|};
  ]

let test2 =
  [
    assert_ __POS__
      "(defn fetch [^java.lang.Integer request ^kotlin.List env context] \
       request)"
      "export const fetch = ((request, env, context) => { return request });";
    assert_ __POS__ "(defn foo [^an.app.Ac act ^an.we.WeVi webView] act)"
      "export const foo = ((act, webView) => { return act });";
    assert_ __POS__ "(defn foo [ ^an.app.Ac act ^an.we.WeVi webView] act)"
      "export const foo = ((act, webView) => { return act });";
    assert_ __POS__ {|(defn foo [^"(App)->aaa.Bbb" a ^"(Baz)->foo.Bar" b] a)|}
      "export const foo = ((a, b) => { return a });";
    assert_ __POS__ {|(defn foo [ ^"(App)->aaa.Bbb" a ^"(Baz)->foo.Bar" b] a)|}
      "export const foo = ((a, b) => { return a });";
    assert_ __POS__ "(defn foo [a b] (let [x (str 'e)] x))"
      {|export const foo = ((a, b) => { return (function () { const x = ("" + e); return x })() });|};
    assert_ __POS__ "(Foo.)" "new Foo()";
    assert_ __POS__ "(Foo. 'a 1)" "new Foo(a, 1)";
    assert_ __POS__ {|(let [fx 1] (fx 2))|}
      {|(function () { const fx = 1; return fx(2) })()|};
    assert_ __POS__ {|(defn f [fx] (fx 2))|}
      {|export const f = ((fx) => { return fx(2) });|};
  ]

let main () =
  [
    ("JS - test1", test1);
    ("JS - test2", test2);
    ( "JS - files",
      [
        assert_file __POS__ "hotreload-client.clj";
        assert_file __POS__ "sample1.clj";
        assert_file __POS__ "main.shared.clj";
      ] );
  ]
