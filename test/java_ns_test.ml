let compile input =
  match Frontend.parse_and_desugar input with
  | Ok sexprs -> Backend_compiler.Java.compile sexprs
  | Error message -> Alcotest.fail message

let no_namespace () =
  let java = compile "(defn test [] 1)" in
  Alcotest.(check string)
    "generated java"
    {|import static y2k.language.language_runtime.*;
public final class user {
static Object test() throws Exception {
return 1;
}
}|}
    java

let require_and_import () =
  let java =
    compile
      {|
(ns app.main
  (:require [io.math.core :as mc])
  (:import [java.time LocalDate]))

(defn test []
  (mc/foo (LocalDate/now)))
|}
  in
  Alcotest.(check ?pos:(Some __POS__) string)
    "generated java"
    {|package app;

import static y2k.language.language_runtime.*;
import java.time.LocalDate;

public final class main {
static Object test() throws Exception {
return io.math.core.foo(LocalDate.now());
}
}|}
    java

let multiple_requires () =
  let java =
    compile
      {|
(ns app.main
  (:require [io.math.core :as mc])
  (:require [app.util :as util]))

(defn test []
  (util/bar (mc/foo 1)))
|}
  in
  Alcotest.(check string)
    "generated java"
    {|package app;

import static y2k.language.language_runtime.*;

public final class main {
static Object test() throws Exception {
return app.util.bar(io.math.core.foo(1));
}
}|}
    java

let namespace_name_only () =
  let java = compile {|
(ns main)

(defn test []
  1)
|} in
  Alcotest.(check string)
    "generated java"
    {|import static y2k.language.language_runtime.*;

public final class main {
static Object test() throws Exception {
return 1;
}
}|}
    java

let string_literals () =
  let java = compile {|
(ns main)

(defn test []
  [:column {:text "Start"}])
|} in
  Alcotest.(check string)
    "generated java"
    {|import static y2k.language.language_runtime.*;

public final class main {
static Object test() throws Exception {
return list("column", hash_map("text", "Start"));
}
}|}
    java

let instance_method_call () =
  let java = compile {|
(defn test [value]
  (.toString value))
|} in
  Alcotest.(check string)
    "generated java"
    {|import static y2k.language.language_runtime.*;
public final class user {
static Object test(Object value) throws Exception {
return value.toString();
}
}|}
    java

let cast () =
  let java = compile {|
(defn test [xs i]
  (.get (cast java.util.List xs) (cast int i)))
|} in
  Alcotest.(check string)
    "generated java"
    {|import static y2k.language.language_runtime.*;
public final class user {
static Object test(Object xs, Object i) throws Exception {
return ((java.util.List) xs).get(((int) i));
}
}|}
    java

let annotated_parameters () =
  let java = compile "(defn test [^java.util.List xs ^int i] (.get xs i))" in
  let has_line prefix = List.exists (String.starts_with ~prefix) (String.split_on_char '\n' java) in
  Alcotest.(check bool) "cast list parameter" true (has_line "var xs = ((java.util.List) G__");
  Alcotest.(check bool) "cast int parameter" true (has_line "var i = ((int) G__");
  Alcotest.(check bool) "uses typed locals" true (has_line "return xs.get(i);")

let constructor_call () =
  let java = compile {|
(defn test []
  (java.time.LocalDate. 2024 1 2))
|} in
  Alcotest.(check string)
    "generated java"
    {|import static y2k.language.language_runtime.*;
public final class user {
static Object test() throws Exception {
return new java.time.LocalDate(2024, 1, 2);
}
}|}
    java

let constructor_call_with_nested_arg () =
  let java = compile {|
(defn test [value]
  (Widget. (.toString value)))
|} in
  Alcotest.(check string)
    "generated java"
    {|import static y2k.language.language_runtime.*;
public final class user {
static Object test(Object value) throws Exception {
return new Widget(value.toString());
}
}|}
    java

let typed_lambda_interop () =
  let java =
    compile
      {|
(defn test []
  (.map (java.util.Optional/of "X")
        ^java.util.function.Function
        (fn [value] (str value "!"))))
|}
  in
  Alcotest.(check string)
    "generated java"
    {|import static y2k.language.language_runtime.*;
public final class user {
static Object test() throws Exception {
return java.util.Optional.of("X").map(((java.util.function.Function)
(value) -> {
try {
return str(value, "!");
} catch (Exception G__1) {
throw sneaky_throw(G__1);
}
}));
}
}|}
    java

let typed_runnable_interop () =
  let java = compile {|
(defn test []
  ^void:java.lang.Runnable
  (fn [] (str "run")))
|} in
  Alcotest.(check string)
    "generated java"
    {|import static y2k.language.language_runtime.*;
public final class user {
static Object test() throws Exception {
return ((java.lang.Runnable)
() -> {
try {
str("run");
} catch (Exception G__2) {
throw sneaky_throw(G__2);
}
});
}
}|}
    java

let typed_consumer_interop () =
  let java = compile {|
(defn test [xs]
  ^void:java.util.function.Consumer
  (fn [value] (.add xs value)))
|} in
  Alcotest.(check string)
    "generated java"
    {|import static y2k.language.language_runtime.*;
public final class user {
static Object test(Object xs) throws Exception {
return ((java.util.function.Consumer)
(value) -> {
try {
xs.add(value);
} catch (Exception G__3) {
throw sneaky_throw(G__3);
}
});
}
}|}
    java

let void_annotation_requires_type () =
  Alcotest.check_raises "void annotation requires type" (Failure "Java lambda annotation ^void: requires a target type")
    (fun () -> ignore (compile {|
(defn test []
  ^void:
  (fn [] nil))
|}))

let gen_class () =
  let java =
    compile
      {|
(ns app.main)

(gen-class
 :name app.main.MainActivity
 :extends android.app.Activity
 :methods [[^override onCreate [android.os.Bundle] void]])

(defn -onCreate [this savedInstanceState]
  nil)
|}
  in
  Alcotest.(check string)
    "generated java"
    {|package app;

import static y2k.language.language_runtime.*;

public final class main {
static Object _onCreate(Object this_, Object savedInstanceState) throws Exception {
return null;
}
public static class MainActivity extends android.app.Activity {
@Override
public void onCreate(android.os.Bundle arg0) {
try {
super.onCreate(arg0);
main._onCreate(this, arg0);
} catch (Exception e) {
throw sneaky_throw(e);
}
}
}
}|}
    java

let () =
  Alcotest.run "java ns"
    [
      ( "JAVA ns",
        [
          Alcotest.test_case "no namespace" `Quick no_namespace;
          Alcotest.test_case "require and import" `Quick require_and_import;
          Alcotest.test_case "multiple requires" `Quick multiple_requires;
          Alcotest.test_case "namespace name only" `Quick namespace_name_only;
          Alcotest.test_case "string literals" `Quick string_literals;
          Alcotest.test_case "instance method call" `Quick instance_method_call;
          Alcotest.test_case "cast" `Quick cast;
          Alcotest.test_case "constructor call" `Quick constructor_call;
          Alcotest.test_case "constructor call with nested arg" `Quick constructor_call_with_nested_arg;
          Alcotest.test_case "typed lambda interop" `Quick typed_lambda_interop;
          Alcotest.test_case "typed runnable interop" `Quick typed_runnable_interop;
          Alcotest.test_case "typed consumer interop" `Quick typed_consumer_interop;
          Alcotest.test_case "annotated parameters" `Quick annotated_parameters;
          Alcotest.test_case "void annotation requires type" `Quick void_annotation_requires_type;
          Alcotest.test_case "gen-class" `Quick gen_class;
        ] );
    ]
