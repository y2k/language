let assert_ code expected =
  let actual = Lib.main_java "src/main.shared.clj" code in
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

let test_file () =
  (* assert_file "main.shared.clj"; *)
  (* assert_file "main.android.clj"; *)
  (* assert_file "interpreter.clj"; *)
  ()

let main () =
  assert_ {|(defn foo [a] a)|}
    {|public static Object foo(final Object a){return a;}|};
  assert_ {|(defn- foo [a] a)|}
    {|private static Object foo(final Object a){return a;}|};
  assert_ {|(defn ^String foo [^int a ^int b] a)|}
    {|public static String foo(final int a,final int b){return a;}|};
  assert_ {|(defn foo [a b] (foo a b) (bar a b))|}
    {|public static Object foo(final Object a,final Object b){foo(a,b);return bar(a,b);}|};
  assert_ {|(defn foo [a b] (a b))|}
    {|public static Object foo(final Object a,final Object b){return a(b);}|};
  assert_ {|(defn foo [[a b]] (a b))|}
    {|public static Object foo(final Object p__1){final var a=get(p__1,0);final var b=get(p__1,1);final var p__2=a(b);return p__2;}|};
  assert_ {|(= a b)|} {|Objects.equals(a,b)|};
  assert_ {|(not= a b)|} {|!Objects.equals(a,b)|};
  assert_ {|(+ a b)|} {|(a+b)|};
  assert_ {|(- a b)|} {|(a-b)|};
  assert_ {|(* a b)|} {|(a*b)|};
  assert_ {|(/ a b)|} {|(a/b)|};
  assert_ {|(> a b)|} {|(a>b)|};
  assert_ {|(< a b)|} {|(a<b)|};
  assert_ {|(>= a b)|} {|(a>=b)|};
  assert_ {|(<= a b)|} {|(a<=b)|};
  assert_ {|[1 2 3]|} {|List.of(1,2,3)|};
  assert_ {|(:webview env)|} {|y2k.RT.get(env,"webview")|};
  assert_ {|(foo a b 1)|} {|foo(a,b,1)|};
  assert_ {|(.foo a b 1)|} {|a.foo(b,1)|};
  assert_ {|(Foo. a b 1)|} {|new Foo(a,b,1)|};
  assert_ {|(String/valueOf level)|} {|String.valueOf(level)|};
  assert_ {|(.join String "" [1 2 3])|} {|String.join("",List.of(1,2,3))|};
  assert_ {|(if a b c)|} {|final Object p__1;if(a){p__1=b;}else{p__1=c;}p__1|};
  assert_ {|(if (if a1 a2 a3) (if b1 b2 b3) (if c1 c2 c3))|}
    {|final Object p__1;if(a1){p__1=a2;}else{p__1=a3;}final Object p__4;if(p__1){final Object p__2;if(b1){p__2=b2;}else{p__2=b3;}p__4=p__2;}else{final Object p__3;if(c1){p__3=c2;}else{p__3=c3;}p__4=p__3;}p__4|};
  assert_ {|(let [^Context wv (foo)] wv)|}
    {|final var wv=(Context)foo();final var p__1=wv;p__1|};
  assert_ {|{:a b :c d}|} {|java.util.Map.of("a",b,"c",d)|};
  assert_ {|(fn [x] x)|} {|(x)->{return x;}|};
  assert_ {|(fn [[a b]] (a b))|}
    {|(p__1)->{final var a=get(p__1,0);final var b=get(p__1,1);final var p__2=a(b);return p__2;}|};
  assert_ {|(foo (if a b c))|}
    {|final Object p__1;if(a){p__1=b;}else{p__1=c;}foo(p__1)|};
  assert_ {|(foo (fn [[a b]] (= a b)))|}
    {|foo((p__1)->{final var a=get(p__1,0);final var b=get(p__1,1);final var p__2=Objects.equals(a,b);return p__2;})|};
  assert_ "(is a List)" "(a instanceof List)";
  assert_ "(as a List)" "(List)a";
  assert_
    {|(ns im.y2k.chargetimer (:import [android.app Activity NotificationChannel])) (defn foo [x] x)|}
    {|package im.y2k.chargetimer;import android.app.Activity;import android.app.NotificationChannel;class Main_shared {public static Object foo(final Object x){return x;}}|};
  assert_
    {|(gen-class
:name WebViewJsListener
:extends Object
:constructors {[Activity WebView] []}
:prefix "wv_"
:methods [[^JavascriptInterface foo [String String] void][^Override bar [int int] String][baz [int int] String]])|}
    {|public static class WebViewJsListener extends Object{public java.util.List<Object> state;public WebViewJsListener(Activity p0,WebView p1){state=java.util.List.of(p0,p1);}@JavascriptInterface public void foo(String p0, String p1){wv_foo(this,p0,p1);}@Override public String bar(int p0, int p1){super.bar(p0,p1);return (String)wv_bar(this,p0,p1);}public String baz(int p0, int p1){return (String)wv_baz(this,p0,p1);}}|};
  assert_ {|(fn [] (bar b c))|} {|()->{return bar(b,c);}|};
  assert_ {|(^void fn [] (bar b c))|} {|()->{bar(b,c);}|};
  assert_ {|(fn! [] (bar b c))|} {|()->{bar(b,c);}|};
  assert_ {|(defn a [] (b) (c) (d))|}
    {|public static Object a(){b();c();return d();}|};
  assert_ {|(let [a 1] (b) (c) (d))|}
    {|final var a=1;b();c();final var p__1=d();p__1|};
  assert_ "(class java.lang.String)" "java.lang.String.class";
  assert_ {|(checked! (foo))|} {|y2k.RT.try_(()->{return foo();})|};
  assert_ {|(get xs 1)|} {|y2k.RT.get(xs,1)|};
  assert_ {|(let [[a] b] a)|}
    {|final var p__1=b;final var a=y2k.RT.get(p__1,0);final var p__2=a;p__2|};
  assert_ {|(let [a (:b c)] a)|}
    {|final var a=y2k.RT.get(c,"b");final var p__1=a;p__1|};
  assert_ {|(str a "b" 3)|} {|y2k.RT.str(a,"b",3)|};
  assert_ {|Context/AUDIO_SERVICE|} {|Context.AUDIO_SERVICE|};
  assert_ {|(foo Context/AUDIO_SERVICE)|} {|foo(Context.AUDIO_SERVICE)|};
  assert_ {|(foo "Context/AUDIO_SERVICE")|} {|foo("Context/AUDIO_SERVICE")|};
  assert_ {|(println a)|} {|System.out.println(y2k.RT.str(a))|};
  assert_ {|(println a 1 "b")|} {|System.out.println(y2k.RT.str(a,1,"b"))|};
  assert_ {|(let [[^Aa a ^Bb b] c] a)|}
    {|final var p__1=c;final var a=(Aa)y2k.RT.get(p__1,0);final var b=(Bb)y2k.RT.get(p__1,1);final var p__2=a;p__2|};
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
  assert_ {|(jvm! (ns im.y2k.c3 (:import [a1.b2 Foo Bar])))|}
    {|package im.y2k.c3;import a1.b2.Foo;import a1.b2.Bar;class Main_shared {}|};
  assert_
    {|(jvm! (ns im.y2k.chargetimer (:import [a.b Ab])))
(jvm! (defn- show_notification [env] (FIXME)))
|}
    {||};
  (* Files *)
  test_file ();
  ()
