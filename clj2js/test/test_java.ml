let assert_ code expected =
  let actual = Lib.main_java "main.clj" code in
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
  assert_file "main.shared.clj";
  (* assert_file "interpreter.clj"; *)
  ()

let main () =
  assert_ {|(defn ^int foo [^int a ^int b] a)|}
    {|public static int foo(final int a,final int b){try{return a;}catch(Exception e){throw new RuntimeException(e);}}|};
  assert_ {|(defn foo [a b] (foo a b) (bar a b))|}
    {|public static Object foo(final Object a,final Object b){try{return bar(a,b);}catch(Exception e){throw new RuntimeException(e);}}|};
  assert_ {|(defn foo [a b] (a b))|}
    {|public static Object foo(final Object a,final Object b){try{return a(b);}catch(Exception e){throw new RuntimeException(e);}}|};
  assert_ {|(defn foo [[a b]] (a b))|}
    {|public static Object foo(final Object p__1){try{final var a=get(p__1,0);final var b=get(p__1,1);final var p__2=a(b);return p__2;}catch(Exception e){throw new RuntimeException(e);}}|};
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
  assert_ {|(:webview env)|} {|get(env,"webview")|};
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
  assert_ {|{:a b :c d}|} {|Map.of("a",b,"c",d)|};
  assert_ {|(fn [x] x)|}
    {|(x)->{try{return x;}catch(Exception e){throw new RuntimeException(e);}}|};
  assert_ {|(fn [[a b]] (a b))|}
    {|(p__1)->{try{final var a=get(p__1,0);final var b=get(p__1,1);final var p__2=a(b);return p__2;}catch(Exception e){throw new RuntimeException(e);}}|};
  assert_ {|(foo (if a b c))|}
    {|final Object p__1;if(a){p__1=b;}else{p__1=c;}foo(p__1)|};
  assert_ {|(foo (fn [[a b]] (= a b)))|}
    {|foo((p__1)->{try{final var a=get(p__1,0);final var b=get(p__1,1);final var p__2=Objects.equals(a,b);return p__2;}catch(Exception e){throw new RuntimeException(e);}})|};
  (*  *)
  (* assert_ {|(defn ^String foo [^int a ^int b] (+ a b) (- a b))|}
     {|public static String foo(int a, int b) { return __prelude_plus(a, b)
      __prelude_minus(a, b) };|}; *)
  (*
        assert_ {|(defn foo [xs] (let [[a b] (foo 1 2)] (bar a b)))|}
          {|fun foo(xs:Any?) = run { val p__1 = foo(1, 2); val a = __prelude_geta(p__1, 0); val b = __prelude_geta(p__1, 1); bar(a, b) };|};
        assert_ {|(get xs 1)|} {|__prelude_geta(xs, 1)|};
        assert_ {|(:foo bar)|} {|__prelude_getm(bar, "foo")|};
        assert_ {|(def foo 1)|} {|val foo = 1;|};
        assert_ {|(def ^:private foo 1)|} {|private val foo = 1;|};
        assert_ {|(.map (listOf "") (fn [x] x))|} {|listOf("").map({ x -> x })|};
        assert_ {|(str 1 "2" 3)|} {|(""+1+"2"+3)|};
        assert_ {|(foo "a\"b")|} {|foo("a\"b")|};
        assert_ {|(__unsafe_inject_code "fun getm() = error(\"require Map\")")|}
          {|fun getm() = error("require Map")|};
        assert_ {|(__unsafe_inject_code "fun foo() = \"a\\\"b\"")|}
          {|fun foo() = "a\"b"|};
        assert_ {|(as x "(Int)->Int")|} {|(x as (Int)->Int)|};
        assert_ {|(spread (.toTypedArray a))|} {|*a.toTypedArray()|};
        assert_ {|(class android.app.AlarmManager)|}
          {|android.app.AlarmManager::class.java|};
        assert_
          {|(ns im.y2k.chargetimer (:import [android.app Activity NotificationChannel]))|}
          {|package im.y2k.chargetimer;
      import android.app.Activity;
      import android.app.NotificationChannel;
      private fun __prelude_plus(a: Any?, b: Any?) = (a as Int) + (b as Int)
      private fun __prelude_minus(a: Any?, b: Any?) = (a as Int) - (b as Int)
      private fun __prelude_getm(x: Any?, y: String): Any? = if (x is Map<*, *>) x.get(y) else error("require Map")
      private fun <T> __prelude_geta(x: List<T>, y: Int): T = x[y]
      private fun __prelude_geta(x: String, y: Int): Char = x[y]|};
        assert_ "(and (= a 1) b)" "(a == 1 && b)";
        assert_ "(or (= a 1) b)" "(a == 1 || b)";
        assert_ "(.play r)" "r.play()";
        assert_ "(. r play)" "r.play()";
        assert_ "(.-play r)" "r.play";
        assert_ "(. r -play)" "r.play";
        assert_ {|(defn foo [a b] a)|} {|fun foo(a:Any?, b:Any?) = run { a };|};
        assert_ {|(defn- foo [a b] a)|}
          {|private fun foo(a:Any?, b:Any?) = run { a };|};
        assert_ "(is node List<*>)" "(node is List<*>)";
        assert_
          {|(gen-class
      :name JobService
      :extends android.app.JobService
      :constructors {[] []}
      :prefix "_"
      :methods [[^Override onStopJob [JobParameters] Boolean]])|}
          {|class JobService() : android.app.JobService() { override fun onStopJob(p0: JobParameters): Boolean = _onStopJob(this, p0) }|};
        assert_
          {|(gen-class
      :name JobService
      :extends Any
      :constructors {[Activity "()->WebView"] []}
      :prefix "_"
      :methods [[^JavascriptInterface foo [JobPar] Unit]])|}
          {|class JobService(p0:Activity, p1:()->WebView) : Any() { val state = listOf<Any>(p0, p1); @JavascriptInterface fun foo(p0: JobPar): Unit = _foo(this, p0) }|};
        assert_ "((foo c d) a b)" "foo(c, d)(a, b)";
        assert_ "[]" "listOf()";
        assert_ "[1 2 3]" "listOf(1,2,3)";
        assert_ "{}" {|mapOf()|};
        assert_ "{:a 1 :b c}" {|mapOf("a" to 1,"b" to c)|};
        assert_ {|(:a b)|} {|__prelude_getm(b, "a")|};
        assert_ "(Foo.)" "Foo()";
        assert_ "(Foo. a 1)" "Foo(a, 1)"; *)
  test_file ();
  ()
