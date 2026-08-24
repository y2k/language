let compile input =
  match Frontend.parse_and_desugar input with
  | Ok sexprs -> Backend_compiler.Js.compile sexprs
  | Error message -> Alcotest.fail message

let require_imports () =
  let js =
    compile
      {|
(ns app.main
  (:require [io.math.core :as mc])
  (:import [java.time LocalDate]))

(defn test []
  (mc/foo 1))
|}
  in
  Alcotest.(check string)
    "generated js"
    {|import { list, vector_QMARK_, concat, hash_map, truthy, print_result, println, eprintln, str, _EQ_, _PLUS_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "./language_runtime.js";
import * as mc from "./io/math/core.js";;
const test = (() => {
return (mc.foo)(1);
});|}
    js

let string_require_import () =
  let js = compile {|
(ns app.main
  (:require ["node:test" :as t])
  (:require ["wrangler" :as w]))
|} in
  Alcotest.(check string)
    "generated js"
    {|import { list, vector_QMARK_, concat, hash_map, truthy, print_result, println, eprintln, str, _EQ_, _PLUS_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "./language_runtime.js";
import * as t from "node:test";
import * as w from "wrangler";;|}
    js

let string_literals () =
  let js = compile {|
(defn test []
  [:column {:text "Start"}])
|} in
  Alcotest.(check string)
    "generated js"
    {|import { list, vector_QMARK_, concat, hash_map, truthy, print_result, println, eprintln, str, _EQ_, _PLUS_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "./language_runtime.js";
const test = (() => {
return (list)("column", (hash_map)("text", "Start"));
});|}
    js

let default_export () =
  let js =
    compile
      {|
(defn handle-fetch [request env ctx]
  (Response. "OK"))

(export-default
 {:fetch (fn [request env ctx]
           (handle-fetch request env ctx))})
|}
  in
  Alcotest.(check string)
    "generated js"
    {|import { list, vector_QMARK_, concat, hash_map, truthy, print_result, println, eprintln, str, _EQ_, _PLUS_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "./language_runtime.js";
const handle_fetch = ((request, env, ctx) => {
return new Response("OK");
});
export default (hash_map)("fetch", ((request, env, ctx) => {
return (handle_fetch)(request, env, ctx);
}));|}
    js

let instance_method_call () =
  let js = compile {|
(defn test [value]
  (.toString value))
|} in
  Alcotest.(check string)
    "generated js"
    {|import { list, vector_QMARK_, concat, hash_map, truthy, print_result, println, eprintln, str, _EQ_, _PLUS_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "./language_runtime.js";
const test = ((value) => {
return value.toString();
});|}
    js

let constructor_call () =
  let js = compile {|
(defn test [title]
  (Widget. title))
|} in
  Alcotest.(check string)
    "generated js"
    {|import { list, vector_QMARK_, concat, hash_map, truthy, print_result, println, eprintln, str, _EQ_, _PLUS_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "./language_runtime.js";
const test = ((title) => {
return new Widget(title);
});|}
    js

let constructor_call_with_nested_arg () =
  let js = compile {|
(defn test [value]
  (Widget. (.toString value)))
|} in
  Alcotest.(check string)
    "generated js"
    {|import { list, vector_QMARK_, concat, hash_map, truthy, print_result, println, eprintln, str, _EQ_, _PLUS_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "./language_runtime.js";
const test = ((value) => {
return new Widget(value.toString());
});|}
    js

let cast_is_no_op () =
  let js = compile "(defn test [value] (cast java.util.List value))" in
  Alcotest.(check string)
    "generated js"
    {|import { list, vector_QMARK_, concat, hash_map, truthy, print_result, println, eprintln, str, _EQ_, _PLUS_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "./language_runtime.js";
const test = ((value) => {
return value;
});|}
    js

let () =
  Alcotest.run "JS ns"
    [
      ( "compiler/ns",
        [
          Alcotest.test_case "require imports" `Quick require_imports;
          Alcotest.test_case "string require import" `Quick string_require_import;
          Alcotest.test_case "string literals" `Quick string_literals;
          Alcotest.test_case "default export" `Quick default_export;
          Alcotest.test_case "instance method call" `Quick instance_method_call;
          Alcotest.test_case "constructor call" `Quick constructor_call;
          Alcotest.test_case "constructor call with nested arg" `Quick constructor_call_with_nested_arg;
          Alcotest.test_case "cast is no-op" `Quick cast_is_no_op;
        ] );
    ]
