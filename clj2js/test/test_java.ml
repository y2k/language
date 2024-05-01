let assert_ code expected =
  let prelude =
    In_channel.with_open_bin
      "../../../test/samples/prelude/java/src/prelude.clj" In_channel.input_all
  in
  let actual =
    Lib__.Frontend.NameGenerator.with_scope (fun _ ->
        Lib.main_java "main.shared.clj" prelude code)
  in
  if actual <> expected then (
    print_endline actual;
    print_newline ();
    failwith "actual <> expected")

let assert_file filename =
  let path = "../../../test/samples/" ^ filename in
  let open In_channel in
  let code = with_open_bin path input_all in
  let expected = with_open_bin (path ^ ".java") input_all in
  assert_ code expected

let assert_files () = assert_file "main.shared.clj"

let assert_strings () =
  assert_ {|(defn foo [a] a)|}
    {|public static Object foo(final Object a){return a;}|};
  assert_ {|(defn- foo [a] a)|}
    {|private static Object foo(final Object a){return a;}|};
  assert_ {|(defn ^String foo [^int a ^int b] a)|}
    {|public static String foo(final int a,final int b){return a;}|};
  assert_ {|(defn foo [a b] (foo a b) (+ a b))|}
    {|public static Object foo(final Object a,final Object b){foo(a,b);return (a+b);}|};
  assert_ {|(defn foo [a b] (a b))|}
    {|public static Object foo(final Object a,final Object b){return a(b);}|};
  assert_ {|(defn foo [[a b]] (a b))|}
    {|public static Object foo(final Object p__1){final var a=get(p__1,0);final var b=get(p__1,1);final var p__2=a(b);return p__2;}|};
  assert_ {|(= 'a 'b)|} {|Objects.equals(a,b)|};
  assert_ {|(not= 'a 'b)|} {|!Objects.equals(a,b)|};
  assert_ {|(+ 'a 'b)|} {|(a+b)|};
  assert_ {|(- 'a 'b)|} {|(a-b)|};
  assert_ {|(* 'a 'b)|} {|(a*b)|};
  assert_ {|(/ 'a 'b)|} {|(a/b)|};
  assert_ {|(> 'a 'b)|} {|(a>b)|};
  assert_ {|(< 'a 'b)|} {|(a<b)|};
  assert_ {|(>= 'a 'b)|} {|(a>=b)|};
  assert_ {|(<= 'a 'b)|} {|(a<=b)|};
  assert_ {|[1 2 3]|} {|List.of(1,2,3)|};
  assert_ {|(:webview 'env)|} {|y2k.RT.get(env,"webview")|};
  assert_ {|(defn foo [a b c] a)(foo 'a 'b 1)|}
    {|public static Object foo(final Object a,final Object b,final Object c){return a;}|};
  assert_ {|(.foo 'a 'b 1)|} {|a.foo(b,1)|};
  assert_ {|(Foo. 'a 'b 1)|} {|new Foo(a,b,1)|};
  assert_ {|(String/valueOf 'level)|} {|String.valueOf(level)|};
  assert_ {|(.join String "" [1 2 3])|} {|String.join("",List.of(1,2,3))|};
  assert_ {|(if 'a 'b 'c)|}
    {|final Object p__1;if(a){p__1=b;}else{p__1=c;}p__1|};
  assert_ {|(if (if 'a1 'a2 'a3) (if 'b1 'b2 'b3) (if 'c1 'c2 'c3))|}
    {|final Object p__1;if(a1){p__1=a2;}else{p__1=a3;}final Object p__4;if(p__1){final Object p__2;if(b1){p__2=b2;}else{p__2=b3;}p__4=p__2;}else{final Object p__3;if(c1){p__3=c2;}else{p__3=c3;}p__4=p__3;}p__4|};
  assert_ {|(defn foo [] (let [^Context wv (foo)] wv))|}
    {|public static Object foo(){final var wv=(Context)foo();final var p__1=wv;return p__1;}|};
  assert_ {|{:a 'b :c 'd}|} {|java.util.Map.of("a",b,"c",d)|};
  assert_ {|(fn [x] x)|} {|(x)->{return x;}|};
  assert_ {|(fn [[a b]] (a b))|}
    {|(p__1)->{final var a=get(p__1,0);final var b=get(p__1,1);final var p__2=a(b);return p__2;}|};
  assert_ {|(str (if 'a 'b 'c))|}
    {|final Object p__1;if(a){p__1=b;}else{p__1=c;}y2k.RT.str(p__1)|};
  assert_ {|(str (fn [[a b]] (= a b)))|}
    {|y2k.RT.str((p__1)->{final var a=get(p__1,0);final var b=get(p__1,1);final var p__2=Objects.equals(a,b);return p__2;})|};
  assert_ "(ns _ (:import [a.b List]))(def b (is 'a List))"
    "package _;import a.b.List;class Main_shared {public static Object b=(a \
     instanceof List);}";
  assert_ "(ns _ (:import [a.b List]))(def b (as 'a List))"
    "package _;import a.b.List;class Main_shared {public static Object \
     b=(List)a;}";
  assert_
    {|(ns im.y2k.chargetimer (:import [android.app Activity NotificationChannel]))(defn foo [x] x)|}
    {|package im.y2k.chargetimer;import android.app.Activity;import android.app.NotificationChannel;class Main_shared {public static Object foo(final Object x){return x;}}|};
  assert_ {|(fn [] (str 'b 'c))|} {|()->{return y2k.RT.str(b,c);}|};
  assert_ {|(^void fn [] (str 'b 'c))|} {|()->{y2k.RT.str(b,c);}|};
  assert_ {|(fn! [] (str 'b 'c))|} {|()->{y2k.RT.str(b,c);}|};
  assert_ {|(defn a [] (str) (str) (str))|}
    {|public static Object a(){y2k.RT.str();y2k.RT.str();return y2k.RT.str();}|};
  assert_ {|(let [a 1] (str) (str) (str))|}
    {|final var a=1;y2k.RT.str();y2k.RT.str();final var p__1=y2k.RT.str();p__1|};
  assert_ "(class java.lang.String)" "java.lang.String.class";
  assert_ {|(checked! (str))|} {|y2k.RT.try_(()->{return y2k.RT.str();})|};
  assert_ {|(get 'xs 1)|} {|y2k.RT.get(xs,1)|};
  assert_ {|(let [[a] 'b] a)|}
    {|final var p__1=b;final var a=y2k.RT.get(p__1,0);final var p__2=a;p__2|};
  assert_ {|(let [a (:b 'c)] a)|}
    {|final var a=y2k.RT.get(c,"b");final var p__1=a;p__1|};
  assert_ {|(str 'a "b" 3)|} {|y2k.RT.str(a,"b",3)|};
  assert_ {|(ns _ (:import [b Context]))(def a Context/AUDIO_SERVICE)|}
    {|package _;import b.Context;class Main_shared {public static Object a=Context.AUDIO_SERVICE;}|};
  assert_ {|(ns _ (:import [b Ctx]))(def a (str Ctx/A_S))|}
    {|package _;import b.Ctx;class Main_shared {public static Object a=y2k.RT.str(Ctx.A_S);}|};
  assert_ {|(str "Context/AUDIO_SERVICE")|}
    {|y2k.RT.str("Context/AUDIO_SERVICE")|};
  assert_ {|(println 'a 1 "b")|} {|y2k.RT.println(a,1,"b")|};
  assert_ {|(defn b [] (let [a 'c] 0))|}
    {|public static Object b(){final var a=c;final var p__1=0;return p__1;}|};
  assert_ {|(defn b [] (let [a 'c] a))|}
    {|public static Object b(){final var a=c;final var p__1=a;return p__1;}|};
  assert_ {|(defn b [] (let [[^Aa a ^Bb b] 'c] a))|}
    {|public static Object b(){final var p__1=c;final var a=(Aa)y2k.RT.get(p__1,0);final var b=(Bb)y2k.RT.get(p__1,1);final var p__2=a;return p__2;}|};
  assert_ {|(comment (defn a [] 1) (defn b [] 2))|} {||};
  assert_ {|(defn c [] 3) (comment (defn a [] 1) (defn b [] 2)) (defn d [] 4)|}
    {|public static Object c(){return 3;}

public static Object d(){return 4;}|};
  assert_ {|(defn a [] 1) (defn b [] 2)|}
    {|public static Object a(){return 1;}
public static Object b(){return 2;}|};
  assert_ {|(jvm! (defn a [] 1) (defn b [] 2))|}
    {|public static Object a(){return 1;}
public static Object b(){return 2;}|};
  assert_ {|(js! (defn a [] 1) (defn b [] 2))|} {||};
  assert_ {|(jvm! (ns im.y2k.c3 (:import [a1.b2 Foo Bar])) (defn foo [a] a))|}
    {|package im.y2k.c3;import a1.b2.Foo;import a1.b2.Bar;class Main_shared {public static Object foo(final Object a){return a;}}|};
  assert_
    {|(jvm! (ns im.y2k.chargetimer (:import [a.b Ab])))
(jvm! (defn- show_notification [env] env))
|}
    {|package im.y2k.chargetimer;import a.b.Ab;class Main_shared {private static Object show_notification(final Object env){return env;}}|};
  assert_ {|(if 'a 'b (println 'c))|}
    {|final Object p__1;if(a){p__1=b;}else{p__1=y2k.RT.println(c);}p__1|};
  assert_ {|(do (str) (println))|} "y2k.RT.str();y2k.RT.println()";
  assert_ {|(let [a (.b 'c 123)] (println "a") (str))|}
    {|final var a=c.b(123);y2k.RT.println("a");final var p__1=y2k.RT.str();p__1|};
  assert_ {|(def LIMIT_CHARGE 80)|} {|public static Object LIMIT_CHARGE=80;|};
  assert_ {|(def- LIMIT_CHARGE 80)|} {|private static Object LIMIT_CHARGE=80;|};
  assert_ {|(def ^int LIMIT_CHARGE 80)|} {|public static int LIMIT_CHARGE=80;|};
  assert_ {|(if false null (throw (Exception. "foo")))|}
    {|final Object p__1;if(false){p__1=null;}else{p__1=y2k.RT.throw_(new Exception("foo"));}p__1|};
  assert_ {|(defn foo [] (str) (println))|}
    {|public static Object foo(){y2k.RT.str();return y2k.RT.println();}|};
  assert_ {|(defn foo [] (if true 1 2) (foo))|}
    {|public static Object foo(){final Object p__1;if(true){p__1=1;}else{p__1=2;};return foo();}|};
  assert_ {|(let [] (str) (println))|}
    {|y2k.RT.str();final var p__1=y2k.RT.println();p__1|};
  assert_ {|(let [] (if true 1 2) (str))|}
    {|final Object p__2;if(true){p__2=1;}else{p__2=2;}final var p__1=y2k.RT.str();p__1|};
  assert_
    {|(gen-class
:name WVJL
:extends Object
:constructors {[Activity WebView] []}
:prefix "wv_"
:methods [[^JavascriptInterface foo [String String] void][^Override bar [int int] String][baz [int int] String]])|}
    {|public static class WVJL extends Object{public java.util.List<Object> state;public WVJL(Activity p0,WebView p1){state=java.util.List.of(p0,p1);}@JavascriptInterface public void foo(String p0, String p1){wv_foo(this,p0,p1);}@Override public String bar(int p0, int p1){super.bar(p0,p1);return (String)wv_bar(this,p0,p1);}public String baz(int p0, int p1){return (String)wv_baz(this,p0,p1);}}|};
  assert_ {|(ns gg.h7.i8
  (:import [a.b C1 C2 C3] [d.e F4 F5 F6]))|}
    {|package gg.h7.i8;import a.b.C1;import a.b.C2;import a.b.C3;import d.e.F4;import d.e.F5;import d.e.F6;|};
  assert_ {|(def a (ClassLoader/getSystemClassLoader))|}
    {|public static Object a=ClassLoader.getSystemClassLoader();|};

  assert_
    {|(ns im.y2k.ch (:import [an.co Ctx AuMa]))
(defn- foo [context] (as (.getSySe context Ctx/AU_SE) AuMa))|}
    {|package im.y2k.ch;import an.co.Ctx;import an.co.AuMa;class Main_shared {private static Object foo(final Object context){return (AuMa)context.getSySe(Ctx.AU_SE);}}|};
  assert_
    {|(ns im.y2k.ch (:import [an.co Ctx AuMa]))
(defn- foo [context] (is (.getSySe context Ctx/AU_SE) AuMa))|}
    {|package im.y2k.ch;import an.co.Ctx;import an.co.AuMa;class Main_shared {private static Object foo(final Object context){return (context.getSySe(Ctx.AU_SE) instanceof AuMa);}}|};
  ()

let main () =
  assert_strings ();
  assert_files ()
