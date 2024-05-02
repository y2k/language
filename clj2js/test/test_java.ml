let assert1 pos code expected =
  let inner_assert () =
    let prelude =
      In_channel.(
        with_open_bin "../../../test/samples/prelude/java/src/prelude.clj"
          input_all)
    in
    let actual =
      Lib__.Frontend.NameGenerator.with_scope (fun _ ->
          Lib.main_java "main.shared.clj" prelude code)
    in
    let start = 0 in
    let actual = String.sub actual start (String.length actual - start) in
    Alcotest.(check ~pos string) "1" expected actual
  in
  Alcotest.test_case "assert_" `Quick inner_assert

let assert_file p filename =
  let path = "../../../test/samples/" ^ filename in
  let open In_channel in
  let code = with_open_bin path input_all in
  let expected = with_open_bin (path ^ ".java") input_all in
  assert1 p code expected

let assert_strings =
  [
    assert1 __POS__ {|(defn foo [a] a)|}
      {|public static Object foo(final Object a){return a;}|};
    assert1 __POS__ {|(defn- foo [a] a)|}
      {|private static Object foo(final Object a){return a;}|};
    assert1 __POS__ {|(defn ^String foo [^int a ^int b] a)|}
      {|public static String foo(final int a,final int b){return a;}|};
    assert1 __POS__ {|(defn foo [a b] (foo a b) (+ a b))|}
      {|public static Object foo(final Object a,final Object b){foo(a,b);return (a+b);}|};
    assert1 __POS__ {|(defn foo [a b] (a b))|}
      {|public static Object foo(final Object a,final Object b){return a(b);}|};
    assert1 __POS__ {|(defn foo [[a b]] (a b))|}
      {|public static Object foo(final Object p__1){final var a=get(p__1,0);final var b=get(p__1,1);final var p__2=a(b);return p__2;}|};
    assert1 __POS__ {|(= 'a 'b)|} {|Objects.equals(a,b)|};
    assert1 __POS__ {|(not= 'a 'b)|} {|!Objects.equals(a,b)|};
    assert1 __POS__ {|(+ 'a 'b)|} {|(a+b)|};
    assert1 __POS__ {|(- 'a 'b)|} {|(a-b)|};
    assert1 __POS__ {|(* 'a 'b)|} {|(a*b)|};
    assert1 __POS__ {|(/ 'a 'b)|} {|(a/b)|};
    assert1 __POS__ {|(> 'a 'b)|} {|(a>b)|};
    assert1 __POS__ {|(< 'a 'b)|} {|(a<b)|};
    assert1 __POS__ {|(>= 'a 'b)|} {|(a>=b)|};
    assert1 __POS__ {|(<= 'a 'b)|} {|(a<=b)|};
    assert1 __POS__ {|[1 2 3]|} {|List.of(1,2,3)|};
    assert1 __POS__ {|(:webview 'env)|} {|y2k.RT.get(env,"webview")|};
    assert1 __POS__ {|(defn foo [a b c] a)(foo 'a 'b 1)|}
      {|public static Object foo(final Object a,final Object b,final Object c){return a;}|};
    assert1 __POS__ {|(.foo 'a 'b 1)|} {|a.foo(b,1)|};
    assert1 __POS__ {|(Foo. 'a 'b 1)|} {|new Foo(a,b,1)|};
    assert1 __POS__ {|(String/valueOf 'level)|} {|String.valueOf(level)|};
    assert1 __POS__ {|(.join String "" [1 2 3])|}
      {|String.join("",List.of(1,2,3))|};
    assert1 __POS__ {|(if 'a 'b 'c)|}
      {|final Object p__1;if(a){p__1=b;}else{p__1=c;}p__1|};
    assert1 __POS__ {|(if (if 'a1 'a2 'a3) (if 'b1 'b2 'b3) (if 'c1 'c2 'c3))|}
      {|final Object p__1;if(a1){p__1=a2;}else{p__1=a3;}final Object p__4;if(p__1){final Object p__2;if(b1){p__2=b2;}else{p__2=b3;}p__4=p__2;}else{final Object p__3;if(c1){p__3=c2;}else{p__3=c3;}p__4=p__3;}p__4|};
    assert1 __POS__ {|(defn foo [] (let [^Context wv (foo)] wv))|}
      {|public static Object foo(){final var wv=(Context)foo();final var p__1=wv;return p__1;}|};
    assert1 __POS__ {|{:a 'b :c 'd}|} {|java.util.Map.of("a",b,"c",d)|};
    assert1 __POS__ {|(fn [x] x)|} {|(x)->{return x;}|};
    assert1 __POS__ {|(fn [[a b]] (a b))|}
      {|(p__1)->{final var a=get(p__1,0);final var b=get(p__1,1);final var p__2=a(b);return p__2;}|};
    assert1 __POS__ {|(str (if 'a 'b 'c))|}
      {|final Object p__1;if(a){p__1=b;}else{p__1=c;}y2k.RT.str(p__1)|};
    assert1 __POS__ {|(str (fn [[a b]] (= a b)))|}
      {|y2k.RT.str((p__1)->{final var a=get(p__1,0);final var b=get(p__1,1);final var p__2=Objects.equals(a,b);return p__2;})|};
    assert1 __POS__ "(ns _ (:import [a.b List]))(def b (is 'a List))"
      "package _;import a.b.List;class Main_shared {public static Object b=(a \
       instanceof List);}";
    assert1 __POS__ "(ns _ (:import [a.b List]))(def b (as 'a List))"
      "package _;import a.b.List;class Main_shared {public static Object \
       b=(List)a;}";
    assert1 __POS__
      {|(ns im.y2k.chargetimer (:import [android.app Activity NotificationChannel]))(defn foo [x] x)|}
      {|package im.y2k.chargetimer;import android.app.Activity;import android.app.NotificationChannel;class Main_shared {public static Object foo(final Object x){return x;}}|};
    assert1 __POS__ {|(fn [] (str 'b 'c))|} {|()->{return y2k.RT.str(b,c);}|};
    assert1 __POS__ {|(^void fn [] (str 'b 'c))|} {|()->{y2k.RT.str(b,c);}|};
    assert1 __POS__ {|(fn! [] (str 'b 'c))|} {|()->{y2k.RT.str(b,c);}|};
    assert1 __POS__ {|(defn a [] (str) (str) (str))|}
      {|public static Object a(){y2k.RT.str();y2k.RT.str();return y2k.RT.str();}|};
    assert1 __POS__ {|(let [a 1] (str) (str) (str))|}
      {|final var a=1;y2k.RT.str();y2k.RT.str();final var p__1=y2k.RT.str();p__1|};
    assert1 __POS__ "(class java.lang.String)" "java.lang.String.class";
    assert1 __POS__ {|(checked! (str))|}
      {|y2k.RT.try_(()->{return y2k.RT.str();})|};
    assert1 __POS__ {|(get 'xs 1)|} {|y2k.RT.get(xs,1)|};
    assert1 __POS__ {|(let [[a] 'b] a)|}
      {|final var p__1=b;final var a=y2k.RT.get(p__1,0);final var p__2=a;p__2|};
    assert1 __POS__ {|(let [a (:b 'c)] a)|}
      {|final var a=y2k.RT.get(c,"b");final var p__1=a;p__1|};
    assert1 __POS__ {|(str 'a "b" 3)|} {|y2k.RT.str(a,"b",3)|};
    assert1 __POS__
      {|(ns _ (:import [b Context]))(def a Context/AUDIO_SERVICE)|}
      {|package _;import b.Context;class Main_shared {public static Object a=Context.AUDIO_SERVICE;}|};
    assert1 __POS__ {|(ns _ (:import [b Ctx]))(def a (str Ctx/A_S))|}
      {|package _;import b.Ctx;class Main_shared {public static Object a=y2k.RT.str(Ctx.A_S);}|};
    assert1 __POS__ {|(str "Context/AUDIO_SERVICE")|}
      {|y2k.RT.str("Context/AUDIO_SERVICE")|};
    assert1 __POS__ {|(println 'a 1 "b")|} {|y2k.RT.println(a,1,"b")|};
    assert1 __POS__ {|(defn b [] (let [a 'c] 0))|}
      {|public static Object b(){final var a=c;final var p__1=0;return p__1;}|};
    assert1 __POS__ {|(defn b [] (let [a 'c] a))|}
      {|public static Object b(){final var a=c;final var p__1=a;return p__1;}|};
    assert1 __POS__ {|(defn b [] (let [[^Aa a ^Bb b] 'c] a))|}
      {|public static Object b(){final var p__1=c;final var a=(Aa)y2k.RT.get(p__1,0);final var b=(Bb)y2k.RT.get(p__1,1);final var p__2=a;return p__2;}|};
    assert1 __POS__ {|(comment (defn a [] 1) (defn b [] 2))|} {||};
    assert1 __POS__
      {|(defn c [] 3) (comment (defn a [] 1) (defn b [] 2)) (defn d [] 4)|}
      {|public static Object c(){return 3;}

public static Object d(){return 4;}|};
    assert1 __POS__ {|(defn a [] 1) (defn b [] 2)|}
      {|public static Object a(){return 1;}
public static Object b(){return 2;}|};
    assert1 __POS__ {|(jvm! (defn a [] 1) (defn b [] 2))|}
      {|public static Object a(){return 1;}
public static Object b(){return 2;}|};
    assert1 __POS__ {|(js! (defn a [] 1) (defn b [] 2))|} {||};
    assert1 __POS__
      {|(jvm! (ns im.y2k.c3 (:import [a1.b2 Foo Bar])) (defn foo [a] a))|}
      {|package im.y2k.c3;import a1.b2.Foo;import a1.b2.Bar;class Main_shared {public static Object foo(final Object a){return a;}}|};
    assert1 __POS__
      {|(jvm! (ns im.y2k.chargetimer (:import [a.b Ab])))
(jvm! (defn- show_notification [env] env))
|}
      {|package im.y2k.chargetimer;import a.b.Ab;class Main_shared {private static Object show_notification(final Object env){return env;}}|};
    assert1 __POS__ {|(if 'a 'b (println 'c))|}
      {|final Object p__1;if(a){p__1=b;}else{p__1=y2k.RT.println(c);}p__1|};
    assert1 __POS__ {|(do (str) (println))|} "y2k.RT.str();y2k.RT.println()";
    assert1 __POS__ {|(let [a (.b 'c 123)] (println "a") (str))|}
      {|final var a=c.b(123);y2k.RT.println("a");final var p__1=y2k.RT.str();p__1|};
    assert1 __POS__ {|(def LIMIT_CHARGE 80)|}
      {|public static Object LIMIT_CHARGE=80;|};
    assert1 __POS__ {|(def- LIMIT_CHARGE 80)|}
      {|private static Object LIMIT_CHARGE=80;|};
    assert1 __POS__ {|(def ^int LIMIT_CHARGE 80)|}
      {|public static int LIMIT_CHARGE=80;|};
    assert1 __POS__ {|(if false null (throw (Exception. "foo")))|}
      {|final Object p__1;if(false){p__1=null;}else{p__1=y2k.RT.throw_(new Exception("foo"));}p__1|};
    assert1 __POS__ {|(defn foo [] (str) (println))|}
      {|public static Object foo(){y2k.RT.str();return y2k.RT.println();}|};
    assert1 __POS__ {|(defn foo [] (if true 1 2) (foo))|}
      {|public static Object foo(){final Object p__1;if(true){p__1=1;}else{p__1=2;};return foo();}|};
    assert1 __POS__ {|(let [] (str) (println))|}
      {|y2k.RT.str();final var p__1=y2k.RT.println();p__1|};
    assert1 __POS__ {|(let [] (if true 1 2) (str))|}
      {|final Object p__2;if(true){p__2=1;}else{p__2=2;}final var p__1=y2k.RT.str();p__1|};
    assert1 __POS__
      {|(gen-class
:name WVJL
:extends Object
:constructors {[Activity WebView] []}
:prefix "wv_"
:methods [[^JavascriptInterface foo [String String] void][^Override bar [int int] String][baz [int int] String]])|}
      {|public static class WVJL extends Object{public java.util.List<Object> state;public WVJL(Activity p0,WebView p1){state=java.util.List.of(p0,p1);}@JavascriptInterface public void foo(String p0, String p1){wv_foo(this,p0,p1);}@Override public String bar(int p0, int p1){super.bar(p0,p1);return (String)wv_bar(this,p0,p1);}public String baz(int p0, int p1){return (String)wv_baz(this,p0,p1);}}|};
    assert1 __POS__ {|(ns gg.h7.i8
  (:import [a.b C1 C2 C3] [d.e F4 F5 F6]))|}
      {|package gg.h7.i8;import a.b.C1;import a.b.C2;import a.b.C3;import d.e.F4;import d.e.F5;import d.e.F6;|};
    assert1 __POS__ {|(def a (ClassLoader/getSystemClassLoader))|}
      {|public static Object a=ClassLoader.getSystemClassLoader();|};
    assert1 __POS__
      {|(ns im.y2k.ch (:import [an.co Ctx AuMa]))
(defn- foo [context] (as (.getSySe context Ctx/AU_SE) AuMa))|}
      {|package im.y2k.ch;import an.co.Ctx;import an.co.AuMa;class Main_shared {private static Object foo(final Object context){return (AuMa)context.getSySe(Ctx.AU_SE);}}|};
    assert1 __POS__
      {|(ns im.y2k.ch (:import [an.co Ctx AuMa]))
(defn- foo [context] (is (.getSySe context Ctx/AU_SE) AuMa))|}
      {|package im.y2k.ch;import an.co.Ctx;import an.co.AuMa;class Main_shared {private static Object foo(final Object context){return (context.getSySe(Ctx.AU_SE) instanceof AuMa);}}|};
    (* *)
    assert1 __POS__ {|(defn foo [] (foo)(foo))|}
      {|public static Object foo(){foo();return foo();}|};
    assert1 __POS__ {|(defn foo [] (foo (foo) (foo)))|}
      {|public static Object foo(){return foo(foo(),foo());}|};
    assert1 __POS__ {|(defn foo [] (if true 2 3))|}
      {|public static Object foo(){final Object p__1;if(true){p__1=2;}else{p__1=3;}return p__1;}|};
    assert1 __POS__ {|(defn foo [] (if true 2 3)(if false 4 5))|}
      {|public static Object foo(){final Object p__1;if(true){p__1=2;}else{p__1=3;};final Object p__2;if(false){p__2=4;}else{p__2=5;}return p__2;}|};
    assert1 __POS__ {|(defn foo [] (if true 2 3)(foo))|}
      {|public static Object foo(){final Object p__1;if(true){p__1=2;}else{p__1=3;};return foo();}|};
    assert1 __POS__ {|(defn foo [x] (let [] (foo 1)(foo 2)))|}
      {|public static Object foo(final Object x){foo(1);final var p__1=foo(2);return p__1;}|};
    assert1 __POS__
      {|(defn foo [x] (let [a (foo 1) b (foo 2)] (foo 3)(foo 4)))|}
      {|public static Object foo(final Object x){final var a=foo(1);final var b=foo(2);foo(3);final var p__1=foo(4);return p__1;}|};
    assert1 __POS__
      {|(defn foo [a b] (foo (if true 2 3) (if true 6 7))(foo (if false 4 5) (if true 8 9)))|}
      {|public static Object foo(final Object a,final Object b){final Object p__1;if(true){p__1=2;}else{p__1=3;}final Object p__2;if(true){p__2=6;}else{p__2=7;}foo(p__1,p__2);final Object p__3;if(false){p__3=4;}else{p__3=5;}final Object p__4;if(true){p__4=8;}else{p__4=9;}return foo(p__3,p__4);}|};
  ]

let main () =
  [
    ("Java - string", assert_strings);
    ("Java - files", [ assert_file __POS__ "main.shared.clj" ]);
  ]
