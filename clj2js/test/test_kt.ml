let assert_ code expected =
  let actual = Lib.main_kt "main.clj" code in
  if actual <> expected then (
    print_endline actual;
    print_newline ();
    failwith "actual <> expected")

let main () =
  assert_ {|(defn ^String foo [^Int a ^Int b] (+ a b) (- a b))|}
    {|fun foo(a:Int, b:Int):String = run { prelude.plus(a, b)
prelude.minus(a, b) };|};
  assert_ {|(defn foo [a b] (+ a b) (- a b))|}
    {|fun foo(a:Any?, b:Any?) = run { prelude.plus(a, b)
prelude.minus(a, b) };|};
  assert_ {|(defn foo [a b] (a b))|} {|fun foo(a:Any?, b:Any?) = run { a(b) };|};
  assert_ {|(defn foo [[a b]] (a b))|}
    {|fun foo(p__1:Any?) = run { val a = geta(p__1, 0); val b = geta(p__1, 1); a(b) };|};
  assert_ {|(defn foo [xs] (let [[a b] (foo 1 2)] (bar a b)))|}
    {|fun foo(xs:Any?) = run { val p__1 = foo(1, 2); val a = geta(p__1, 0); val b = geta(p__1, 1); bar(a, b) };|};
  assert_ {|(= a b)|} {|a == b|};
  assert_ {|(not= a b)|} {|a != b|};
  assert_ {|(get xs 1)|} {|geta(xs, 1)|};
  assert_ {|(:foo bar)|} {|getm(bar, "foo")|};
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
import android.app.NotificationChannel;|};
  assert_ {|(ns prelude)|} {|package prelude;|};
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
:constructors {[Activity WebView] []}
:prefix "_"
:methods [[^JavascriptInterface foo [JobPar] Unit]])|}
    {|class JobService(p0:Activity, p1:WebView) : Any() { val state = listOf<Any>(p0, p1); @JavascriptInterface fun foo(p0: JobPar): Unit = _foo(this, p0) }|};
  assert_
    {|(proxy [] []
JavascriptInterface
(dispatch [_ ^String event ^Any payload]
  (.runOnUiThread activity (fn [] (dispatch event payload)))))|}
    {|object  {
@JavascriptInterface
fun dispatch (event:String, payload:Any) { activity.runOnUiThread({  dispatch(event, payload) }) }}|};
  assert_ "((foo c d) a b)" "foo(c, d)(a, b)";
  ()
