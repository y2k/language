let assert_ = Utils.assert_ (Lib.main_js true) "js/src/prelude.clj"

let assert_with_import =
  Utils.assert_with_import (Lib.main_js true) "js/src/prelude.clj"

let assert_file =
  Utils.assert_file (Lib.main_js true) "js/src/prelude.clj" ".js"

let _test00 () =
  let lines =
    In_channel.with_open_bin "../../../test/samples/default_js.txt"
      In_channel.input_lines
  in
  let result =
    lines
    |> List.map (fun line ->
           print_endline @@ "COMPILE: " ^ line;
           Utils.compile_code (Lib.main_js true) "js/src/prelude.clj" line)
    |> List.fold_left (Printf.sprintf "%s\n\n%s") ""
  in
  Out_channel.with_open_bin "../../../test/samples/default_js.out.txt"
    Out_channel.output_string result;
  []

(*  *)
let test0 =
  [
    (* assert_ __POS__ "(try 1 (catch :default e 2 3 e))" ""; *)
    assert_ __POS__ "(ns _) (defn foo [a] (fetch (spread a)))"
      "export const foo = ((a) => { const p__6 = a;;\nreturn fetch(...p__6) });";
    assert_ __POS__ {|(fetch (spread [1 2 3]))|}
      "const p__6 = [1, 2, 3];\nfetch(...p__6)";
    assert_ __POS__ {|(fetch (spread (alert [1 2 3])))|}
      "const p__6 = [1, 2, 3];\nconst p__7 = alert(p__6);\nfetch(...p__7)";
    assert_ __POS__ {|(fetch [1 2 3])|} {|const p__6 = [1, 2, 3];
fetch(p__6)|};
    assert_ __POS__ {|(fetch {:a (if 1 2 3)})|}
      {|let p__9;
if (1) {
p__9 = 2;
} else {
p__9 = 3;
}
const p__7 = p__9;
const p__6 = {["a"]: p__7};
fetch(p__6)|};
    assert_ __POS__ {|(let [a (fetch 1)] a)|} {|const a = fetch(1);
a|};
    assert_ __POS__ {|(let [a (fetch (alert 1))] a)|}
      {|const p__6 = alert(1);
const a = fetch(p__6);
a|};
    assert_ __POS__ {|[(if 1 2 3)]|}
      "let p__8;\n\
       if (1) {\n\
       p__8 = 2;\n\
       } else {\n\
       p__8 = 3;\n\
       }\n\
       const p__6 = p__8;\n\
       [p__6]";
    assert_ __POS__ {|{:a (if 4 5 6)}|}
      "let p__8;\n\
       if (4) {\n\
       p__8 = 5;\n\
       } else {\n\
       p__8 = 6;\n\
       }\n\
       const p__6 = p__8;\n\
       {[\"a\"]: p__6}";
    assert_ __POS__ {|{(if 1 2 3) (if 4 5 6)}|}
      "let p__8;\n\
       if (1) {\n\
       p__8 = 2;\n\
       } else {\n\
       p__8 = 3;\n\
       }\n\
       const p__6 = p__8;\n\
       let p__11;\n\
       if (4) {\n\
       p__11 = 5;\n\
       } else {\n\
       p__11 = 6;\n\
       }\n\
       const p__9 = p__11;\n\
       {[p__6]: p__9}";
    assert_ __POS__ {|(let [] (fn [] (if false 1 2)))|}
      "(() => { let p__7;;\n\
       if (false) {\n\
       p__7 = 1;\n\
       } else {\n\
       p__7 = 2;\n\
       };\n\
       return p__7 })";
    assert_ __POS__ {|(foo (bar))|} {|const p__6 = bar();
foo(p__6)|};
    assert_ __POS__ {|(foo (bar (oof)))|}
      "const p__7 = oof();\nconst p__6 = bar(p__7);\nfoo(p__6)";
    assert_ __POS__ {|(defn foo [bar then] (bar 1) (then (bar 2)))|}
      "export const foo = ((bar, then) => { bar(1);\n\
       const p__6 = bar(2);;\n\
       return then(p__6) });";
    (* assert_ __POS__ {|(export-default {:fetch (fn [a] a)})|} {|export default {["fetch"]: ((a) => { return a })}|}; *)
    (* assert_ __POS__ {|(cond (= 1 2) 3 (= 4 5) 6 :else 0)|} {||}; *)
    (* assert_ __POS__ {|(if 1 2 3)|} {|(1 ? 2 : 3)|}; *)
    assert_ __POS__ {|(fetch (if (alert 1) (alert 21 (alert 22)) (alert 3)))|}
      "let p__8;\n\
       const p__7 = alert(1);\n\
       if (p__7) {\n\
       const p__9 = alert(22);\n\
       p__8 = alert(21, p__9);\n\
       } else {\n\
       p__8 = alert(3);\n\
       }\n\
       const p__6 = p__8;\n\
       fetch(p__6)";
    assert_ __POS__ {|(cond 1 2 3 4 :else 5)|}
      "let p__7;\n\
       if (1) {\n\
       p__7 = 2;\n\
       } else {\n\
       let p__9;\n\
       if (3) {\n\
       p__9 = 4;\n\
       } else {\n\
       p__9 = 5;\n\
       }\n\
       p__7 = p__9;\n\
       }\n\
       p__7";
  ]

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
    (* assert_ __POS__ "(let [r 0] (.json r))" "const r=0;\nr.json()";
       assert_ __POS__ "(let [r 0 a 1] (.json r a))"
         "(function () { const r = 0; const a = 0; return r.json(a) })()"; *)
    assert_ __POS__ "(let [r 0 a 1 b 2] (.json r a b))"
      {|const r = 0;
const a = 1;
const b = 2;
r.json(a, b)|};
    assert_ __POS__
      {|(defn fetch [request env context]
                (.log console request)
                (.log console request))|}
      {|export const fetch = ((request, env, context) => { console.log(request);
return console.log(request) });|};
    assert_ __POS__ {|(defn b [x] x) (defn fetch [r e c] (-> 0 b))|}
      "export const b = ((x) => { return x });\n\
       export const fetch = ((r, e, c) => { return b(0) });";
    assert_ __POS__
      {|(.next
          (.json :request null)
          (fn [text] (println "hello")))|}
      "const p__6 = \"request\".json(null);\n\
       const p__7 = ((text) => { return console.info(\"hello\") });\n\
       p__6.next(p__7)";
    assert_ __POS__
      {|(defn fetch [request env context]
          (->
            (.json request null)
            (.next (fn [text] (println "hello")))))|}
      "export const fetch = ((request, env, context) => { const p__6 = \
       request.json(null);;\n\
       const p__7 = ((text) => { return console.info(\"hello\") });;\n\
       return p__6.next(p__7) });";
    assert_ __POS__
      {|(defn fetch [request env context]
        request)|}
      "export const fetch = ((request, env, context) => { return request });";
    assert_ __POS__ {|(export-default {:fetch :fetch_handler})|}
      "const p__6 = {[\"fetch\"]: \"fetch_handler\"};\nexport default p__6";
    assert_ __POS__ {|(defn- foo[x]x)(export-default {:fetch foo})|}
      "const foo = ((x) => { return x });\n\
       const p__6 = {[\"fetch\"]: foo};\n\
       export default p__6";
    assert_ __POS__
      {|(defn fetch-handler [request env context] request)
     (export-default {:fetch fetch-handler})|}
      "export const fetch-handler = ((request, env, context) => { return \
       request });\n\
       const p__6 = {[\"fetch\"]: fetch-handler};\n\
       export default p__6";
    assert_ __POS__ "(println 1 2 3)" "console.info(1, 2, 3)";
    assert_ __POS__ "(defn foo [x] 0) (foo (println 1 2 3))"
      {|export const foo = ((x) => { return 0 });
const p__6 = console.info(1, 2, 3);
foo(p__6)|};
    assert_ __POS__
      {|(comment 1 2 3)
     (println 1 2 3)
     (comment 1 2 3)|}
      "console.info(1, 2, 3)";
    assert_ __POS__
      {|(export-default {:foo 1 :foo2 {:foo 1 :bar "2" :baz false} :bar "2" :baz false})|}
      {|export default {["foo"]: 1, ["foo2"]: {["foo"]: 1, ["bar"]: "2", ["baz"]: false}, ["bar"]: "2", ["baz"]: false}|};
    assert_ __POS__
      {|(if (fetch 1) 0 1)
        (if (if (fetch :c0) 2 3)
          (if (fetch :c1) 4 5)
          (if (fetch :c2) 6 7))
        (if (= 1 2) 8 9)|}
      {|let p__6;
const p__5 = fetch(1);
if (p__5) {
p__6 = 0;
} else {
p__6 = 1;
}
p__6
let p__10;
let p__7;
const p__7 = fetch("c0");
if (p__7) {
p__7 = 2;
} else {
p__7 = 3;
}
const p__9 = p__7;
if (p__9) {
let p__14;
const p__13 = fetch("c1");
if (p__13) {
p__14 = 4;
} else {
p__14 = 5;
}
p__10 = p__14;
} else {
let p__12;
const p__11 = fetch("c2");
if (p__11) {
p__12 = 6;
} else {
p__12 = 7;
}
p__10 = p__12;
}
p__10
let p__16;
const p__15 = 1 === 2;
if (p__15) {
p__16 = 8;
} else {
p__16 = 9;
}
p__16|};
    assert_ __POS__ {|(+ 1 2)|} {|(1 + 2)|};
    assert_ __POS__ "(+ 1 (+ 10 20 30) 3 (str 1) 5)"
      {|(1 + (10 + 20 + 30) + 3 + ("" + 1) + 5)|};
    assert_ __POS__ "(- 1 (- 10 20))" "(1 - (10 - 20))";
    assert_ __POS__ "(- 1 2 3 4)" "(1 - 2 - 3 - 4)";
    assert_ __POS__ {|{"content-type" "application/json" :a [1 [10 20 30] 3]}|}
      {|{["content-type"]: "application/json", ["a"]: [1, [10, 20, 30], 3]}|};
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
      {|(defn map[a b]0)(map (fn [] 0) :xs)
   (map (fn [x] x) :xs)|}
      {|export const map = ((a, b) => { return 0 });
map((() => { return 0 }), "xs")
map(((x) => { return x }), "xs")|};
    assert_ __POS__ "(and 1)" "(1)";
    assert_ __POS__ "(and 1 2)" "(1 && 2)";
    assert_ __POS__ {|(and :a (and 1 2 3) :c :d)|}
      {|("a" && (1 && 2 && 3) && "c" && "d")|};
    assert_ __POS__ "(< :x 1)(> :y 2)" {|("x" < 1)
("y" > 2)|};
    assert_ __POS__ "(<= 3 1)(>= 4 2)" "(3 <= 1)\n(4 >= 2)";
    assert_ __POS__ "(def foo (+ 1 2))" "export const foo = (1 + 2);";
    assert_ __POS__ "(let [b 1 a b?.c?.d?.e] a)"
      "(function () { const b = 1; const a = b?.c?.d?.e; return a })()";
    assert_ __POS__ "(let [b 1 a b.c.d.e] a)"
      "(function () { const b = 1; const a = b.c.d.e; return a })()";
    assert_ __POS__ "(or 1)" "(1)";
    assert_ __POS__ "(or :a :b 1 2)" {|("a" || "b" || 1 || 2)|};
    assert_ __POS__ "(or :a (fn [x] x) 1)"
      {|("a" || ((x) => { return x }) || 1)|};
    assert_ __POS__ "(alert 1)\n\n\n(alert 3)" "alert(1)\nalert(3)";
    assert_ __POS__ "(alert 1)\n;;(alert 2)\n(alert 3)" "alert(1)\nalert(3)";
    assert_ __POS__ "(alert 1)\n;;(alert 2.1)\n;;(alert 2.2)\n(alert 3)"
      "alert(1)\nalert(3)";
    assert_ __POS__ "(alert 1)\n\n;;(alert 2.1)\n;;(alert 2.2)\n\n(alert 3)"
      "alert(1)\nalert(3)";
    assert_ __POS__ "(alert 1)\n\n;;(alert 2.1)\n\n;;(alert 2.2)\n\n(alert 3)"
      "alert(1)\nalert(3)";
    assert_ __POS__ "(->> 0 (alert 1) (alert 2) (alert 3))"
      "alert(3, alert(2, alert(1, 0)))";
    assert_ __POS__ "(if-let [a 1 b 2 c 3] alert :bar)"
      "(function () { const a = 1; return (a ? (function () { const b = 2; \
       return (b ? (function () { const c = 3; return (c ? alert : \"bar\") \
       })() : \"bar\") })() : \"bar\") })()";
    assert_ __POS__ "(if-let [a 1 b 2 c 3] (+ a b c) -1)"
      "(function () { const a = 1; return (a ? (function () { const b = 2; \
       return (b ? (function () { const c = 3; return (c ? (a + b + c) : -1) \
       })() : -1) })() : -1) })()";
    assert_ __POS__ "(if-let [_ 1 _ 2 _ 3] 6 -1)"
      "(function () { const _ = 1; return (_ ? (function () { const _ = 2; \
       return (_ ? (function () { const _ = 3; return (_ ? 6 : -1) })() : -1) \
       })() : -1) })()";
    assert_ __POS__ "(if-let* [a 1 b 2 c 3] :foo :bar)"
      "(function () { const a = 1; return (a ? (function () { const b = 2; \
       return (b ? (function () { const c = 3; return (c ? \"foo\" : \"bar\") \
       })() : \"bar\") })() : \"bar\") })()";
    assert_ __POS__ "(if-let* [a 1 b 2 c 3] (+ a b c) -1)"
      "(function () { const a = 1; return (a ? (function () { const b = 2; \
       return (b ? (function () { const c = 3; return (c ? (a + b + c) : -1) \
       })() : -1) })() : -1) })()";
    assert_ __POS__ "(if-let* [_ 1 _ 2 _ 3] 6 -1)"
      "(function () { const _ = 1; return (_ ? (function () { const _ = 2; \
       return (_ ? (function () { const _ = 3; return (_ ? 6 : -1) })() : -1) \
       })() : -1) })()";
    assert_ __POS__ {|(def- a {})(assoc a :city "NY")|}
      {|const a = {};
{ ...a, ["city"]: "NY" }|};
    assert_ __POS__ {|(def- a {})(assoc a :user_id :data.now)|}
      {|const a = {};
{ ...a, ["user_id"]: "data.now" }|};
    assert_ __POS__ {|(-> (alert :person) (assoc :city "NY"))|}
      {|{ ...alert("person"), ["city"]: "NY" }|};
    assert_ __POS__ {|(-> :person (assoc :city "NY"))|}
      {|{ ..."person", ["city"]: "NY" }|};
    assert_ __POS__ "(def- a {})(def- b {})(merge a b)"
      "const a = {};\nconst b = {};\n{ ...a, ...b }";
    assert_ __POS__ "(alert (spread [1 2 3]))" "alert(...[1, 2, 3])";
    assert_ __POS__ "(conj [1 2] 3)" "[...[1, 2], 3]";
    assert_ __POS__ "(concat [1 2] [3 4])" "[...[1, 2], ...[3, 4]]";
    assert_ __POS__ "[]" "[]";
    assert_ __POS__ "{}" "{}";
    assert_ __POS__ {|(throw (Error. "foo"))|}
      {|(function(){throw new Error("foo")})()|};
    assert_ __POS__ "(not= 1 2)" "!(1 === 2)";
    assert_ __POS__ "(/ 5 (/ 17 3))" "(5 / (17 / 3))";
    assert_ __POS__ "(* 1 (* 2 3 4))" "(1 * (2 * 3 * 4))";
    assert_ __POS__ "(defn- foo [x] x)" "const foo = ((x) => { return x });";
    assert_ __POS__ "(defn foo [x] x)"
      "export const foo = ((x) => { return x });";
    assert_ __POS__ "(alert (FIXME))"
      {|alert((function(){throw new Error(("" + "FIXME main.clj:1:8 - "))})())|};
    assert_ __POS__ "(alert (FIXME :A1 2))"
      {|alert((function(){throw new Error(("" + "FIXME main.clj:1:8 - " + "A1" + 2))})())|};
    assert_ __POS__ {|(println)|} {|console.info()|};
    assert_ __POS__ {|(def- a 1)(println "hello" a 123)|}
      {|const a = 1;
console.info("hello", a, 123)|};
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
    assert_ __POS__ "(do (alert 1 2) (alert 3 4) (alert 5 6))"
      "(function () { alert(1, 2); alert(3, 4); return alert(5, 6) })()";
    assert_ __POS__ "(str :a (if :b :c :d))" {|("" + "a" + ("b" ? "c" : "d"))|};
    assert_ __POS__ "(set! :foo 1)" {|("foo" = 1);|};
    assert_ __POS__ "(def- a {})(set! a.bar 1)" "const a = {};\n(a.bar = 1);";
    assert_ __POS__ "(def- a {})(set! (.-bar a) 1)"
      "const a = {};\n(a.bar = 1);";
    assert_ __POS__ "(set! (.-bar (alert 2)) 1)" "(alert(2).bar = 1);";
    assert_ __POS__ "(def- a {})(set! (.-bar (get a 2)) 1)"
      "const a = {};\n(a[2].bar = 1);";
    assert_ __POS__ "(defmacro foo [a b] (list a 1)) (foo alert d)" "alert(1)";
    assert_ __POS__
      "(defmacro foo [a b] (list 'do (list a 1) (list b 2))) (foo alert alert)"
      "(function () { alert(1); return alert(2) })()";
    assert_ __POS__ "(type 1)" "typeof 1";
    assert_ __POS__ {|(def- a "")(= (type a) "String")|}
      {|const a = "";
typeof a === "String"|};
    assert_ __POS__ "(not 1)" "!(1)";
    assert_ __POS__ "(not (+ 1 2))" "!((1 + 2))";
    assert_ __POS__ "(def- a [])(get a 7)" "const a = [];\na[7]";
    assert_ __POS__ "(case (alert 1) 2 (alert 22) (alert 3) :baz :qwe 3 :other)"
      {|(function () { const gen_1 = alert(1); return (gen_1 === 2 ? alert(22) : (gen_1 === alert(3) ? "baz" : (gen_1 === "qwe" ? 3 : "other"))) })()|};
    assert_ __POS__ "(case :key 2 (alert 22) (alert 3) :baz :qwe 3 :other)"
      {|(function () { const gen_1 = "key"; return (gen_1 === 2 ? alert(22) : (gen_1 === alert(3) ? "baz" : (gen_1 === "qwe" ? 3 : "other"))) })()|};
    assert_ __POS__ "(case :key 2 (alert 22) (alert 3) :baz :qwe 3 (alert :a))"
      {|(function () { const gen_1 = "key"; return (gen_1 === 2 ? alert(22) : (gen_1 === alert(3) ? "baz" : (gen_1 === "qwe" ? 3 : alert("a")))) })()|};
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
    assert_ __POS__ {|(def- a {})(assoc! a.db 7 a.now)|}
      "const a = {};\na.db[7]=a.now";
    assert_ __POS__ "[:div.tgme]" {|["div.tgme"]|};
    assert_ __POS__ "{:div.tgme :foo}" {|{["div.tgme"]: "foo"}|};
    assert_ __POS__ "{:div :foo}" {|{["div"]: "foo"}|};
    assert_ __POS__ "(^export def foo (+ 1 2))" "export const foo = (1 + 2);";
    assert_ __POS__ {|(alert "foo\"bar")|} {|alert("foo\"bar")|};
    assert_ __POS__ "(% 1 2)" "(1 % 2)";
    assert_ __POS__ {|(alert "a\"b")|} {|alert("a\"b")|};
    assert_ __POS__ "(def- r {})(.play r)(. r play)"
      "const r = {};\nr.play()\nr.play()";
    assert_ __POS__ "(def- r {})(.-play r)" "const r = {};\nr.play";
    assert_ __POS__ "(def- r {})(. r -play)" "const r = {};\nr.play";
    assert_ __POS__ {|{:headers {:get (fn [] "TG_SECRET_TOKEN")}}|}
      {|{["headers"]: {["get"]: (() => { return "TG_SECRET_TOKEN" })}}|};
    assert_ __POS__ {|(cond (str "c") (str "a") :else (str "b"))|}
      {|(("" + "c") ? ("" + "a") : ("" + "b"))|};
    assert_ __POS__ {|[(str "a")]|} {|[("" + "a")]|};
    assert_ __POS__ "((alert :c :d) :a :b)" {|alert("c", "d")("a", "b")|};
    assert_ __POS__ {|(let [[a b] :c] a)|}
      {|(function () { const p__1 = "c"; const a = p__1[0]; const b = p__1[1]; return a })()|};
    assert_ __POS__ {|(def- b {})(:a b)|} {|const b = {};
b["a"]|};
    assert_ __POS__ {|(jvm! (def a 1) (def b 2))|} {||};
    assert_ __POS__ {|(js! (def a 1) (def b 2))|}
      "export const a = 1;\nexport const b = 2;";
    assert_ __POS__ {|(fn [{a :url b :props}] [a b])|}
      {|((p__1) => { return (function () { const a = p__1["url"]; const b = p__1["props"]; return [a, b] })() })|};
    assert_ __POS__ {|(atom 1)|} {|Array.of(1)|};
    assert_ __POS__ {|(def- x 1)(deref x)|} {|const x = 1;
x[0]|};
    assert_ __POS__ {|(def- x 1)(reset! x 2)|}
      {|const x = 1;
(function () { x.fill(2); return 2 })()|};
    assert_ __POS__ {|(def- a {})(swap! a (fn [x] x))|}
      {|const a = {};
a.splice(0, 1, (((x) => { return x }))(a[0]))[0]|};
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
    assert_ __POS__ {|(.join :r)|} {|"r".join()|};
    assert_ __POS__ {|[(.join :r)]|} {|["r".join()]|};
    assert_ __POS__ {|{:b (.join :r)}|} {|{["b"]: "r".join()}|};
    assert_ __POS__
      {|(defn- tr [user] (let [un "c"] (str "a" un "b" user.id ")")))|}
      {|const tr = ((user) => { return (function () { const un = "c"; return ("" + "a" + un + "b" + user.id + ")") })() });|};
    assert_ __POS__
      {|(defn- tr [user] {:cp (let [un "c"] (str "a" un "b" user.id ")"))})|}
      {|const tr = ((user) => { return {["cp"]: (function () { const un = "c"; return ("" + "a" + un + "b" + user.id + ")") })()} });|};
    assert_ __POS__ {|(defn f [x] x)(f 1)|}
      "export const f = ((x) => { return x });\nf(1)";
    assert_ __POS__ {|(defn f [x y] y)(f 1 2)|}
      "export const f = ((x, y) => { return y });\nf(1, 2)";
    assert_ __POS__ "(defn- f [a b & c] 0)(f 1 2)"
      "const f = ((a, b, ...c) => { return 0 });\nf(1, 2)";
    assert_ __POS__ "(defn- f [a b & c] 0)(f 1 2 3)"
      "const f = ((a, b, ...c) => { return 0 });\nf(1, 2, 3)";
    assert_ __POS__ "(defn- f [a b & c] 0)(f 1 2 3 4)"
      "const f = ((a, b, ...c) => { return 0 });\nf(1, 2, 3, 4)";
    assert_ __POS__ {|((= 3 "https://g.com/a") 1)|}
      {|3 === "https://g.com/a"(1)|};
    assert_ __POS__ "(.-subtle crypto)" "crypto.subtle";
    assert_ __POS__ {|{:a 1}|} {|{["a"]: 1}|};
    assert_ __POS__ {|(let [b "c"] {b 2})|}
      {|(function () { const b = "c"; return {[b]: 2} })()|};
    assert_ __POS__
      {|(defmacro foo [xs] (list '.at (list '.from 'Array xs) -1)) (foo [])|}
      {|Array.from([]).at(-1)|};
    assert_ __POS__ {|(alert globalThis)|} {|alert(globalThis)|};
    assert_ __POS__ {|(alert (quote a))|}
      {|alert({["__y2k_type"]: "quote", ["value"]: "a"})|};
    assert_ __POS__ {|(alert 'a)|}
      {|alert({["__y2k_type"]: "quote", ["value"]: "a"})|};
    assert_ __POS__ {|(FIXME)|}
      {|(function(){throw new Error(("" + "FIXME main.clj:1:1 - "))})()|};
    assert_ __POS__ {|__POS__|} {|"main.clj:1:1"|};
  ]

let linter_tests =
  [
    assert_ __POS__ {|(defn foo [x] x)(defn bar [y] (foo y))|}
      "export const foo = ((x) => { return x });\n\
       export const bar = ((y) => { return foo(y) });";
    assert_ __POS__ {|(defn bar [y] y.b)|}
      {|export const bar = ((y) => { return y.b });|};
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
    assert_ __POS__ "(defn foo [a b] (let [x (str :e)] x))"
      {|export const foo = ((a, b) => { return (function () { const x = ("" + "e"); return x })() });|};
    assert_ __POS__ "(Foo.)" "new Foo()";
    assert_ __POS__ "(Foo. :a 1)" "new Foo(\"a\", 1)";
    assert_ __POS__ {|(let [fx 1] (fx 2))|}
      {|(function () { const fx = 1; return fx(2) })()|};
    assert_ __POS__ {|(defn f [fx] (fx 2))|}
      {|export const f = ((fx) => { return fx(2) });|};
    assert_ __POS__ "(ns html)" "";
    assert_ __POS__ {|(fn [a e] (+ a e))|} {|((a, e) => { return (a + e) })|};
    assert_ __POS__ {|(fn [{a :b c :d} e] (+ a c e))|}
      {|((p__1, e) => { return (function () { const a = p__1["b"]; const c = p__1["d"]; return (a + c + e) })() })|};
    assert_ __POS__ {|(fn [[{a :b c :d} e]] (+ a c e))|}
      "((p__1) => { return (function () { const p__2 = p__1[0]; const a = \
       p__2[\"b\"]; const c = p__2[\"d\"]; const e = p__1[1]; return (a + c + \
       e) })() })";
    assert_ __POS__ {|(defn- foo [r e w]0)(export-default {:fetch foo})|}
      {|const foo = ((r, e, w) => { return 0 });
export default {["fetch"]: foo}|};
  ]

let main () =
  [
    (* ("JS - test00", _test00); *)
    ("JS - test0", test0);
    ("JS - test1", test1);
    ("JS - test2", test2);
    ("JS - Linter tests", linter_tests);
    ( "JS - files",
      [
        assert_file __POS__ "hotreload-client.clj";
        assert_file __POS__ "sample1.clj";
        assert_file __POS__ "main.shared.clj";
      ] );
  ]
