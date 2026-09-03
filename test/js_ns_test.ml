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
   (:require [effect-fetch :as fetch])
   (:require [effects-promise.fetch :as promise])
   (:import [java.time LocalDate]))

(defn test []
  (mc/foo 1))
|}
  in
  Alcotest.(check string)
    "generated js"
    {|import { list, vector_QMARK_, concat, hash_map, truthy, not, print_result, println, eprintln, str, _EQ_, _PLUS_, _GT_, _LT_, _GT__EQ_, _LT__EQ_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "../language_runtime.js";
import * as mc from "../io/math/core.js";
import * as fetch from "../effect_fetch.js";
import * as promise from "../effects_promise/fetch.js";;
export const test = (() => {
return (mc.foo)(1);
});|}
    js

let string_require_import () =
  let js =
    compile
      {|
(ns app.main
   (:require ["node:test" :as t])
   (:require ["wrangler" :as w])
   (:require ["node:async_hooks" :as async-hooks]))
|}
  in
  Alcotest.(check string)
    "generated js"
    {|import { list, vector_QMARK_, concat, hash_map, truthy, not, print_result, println, eprintln, str, _EQ_, _PLUS_, _GT_, _LT_, _GT__EQ_, _LT__EQ_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "../language_runtime.js";
import * as t from "node:test";
import * as w from "wrangler";
import * as async_hooks from "node:async_hooks";;|}
    js

let root_namespace_imports () =
  let js = compile {|
(ns main (:require [db :as db]))
|} in
  Alcotest.(check string)
    "generated js"
    {|import { list, vector_QMARK_, concat, hash_map, truthy, not, print_result, println, eprintln, str, _EQ_, _PLUS_, _GT_, _LT_, _GT__EQ_, _LT__EQ_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "./language_runtime.js";
import * as db from "./db.js";;|}
    js

let nested_namespace_imports () =
  let js =
    compile
      {|
(ns app.commands.add
   (:require [app.commands :as parent])
   (:require [app.commands.remove :as sibling])
   (:require [app.commands.add.audit :as child])
   (:require [other.feature.worker :as foreign]))
|}
  in
  Alcotest.(check string)
    "generated js"
    {|import { list, vector_QMARK_, concat, hash_map, truthy, not, print_result, println, eprintln, str, _EQ_, _PLUS_, _GT_, _LT_, _GT__EQ_, _LT__EQ_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "../../language_runtime.js";
import * as parent from "../../app/commands.js";
import * as sibling from "../../app/commands/remove.js";
import * as child from "../../app/commands/add/audit.js";
import * as foreign from "../../other/feature/worker.js";;|}
    js

let string_literal_with_slash () =
  let js = compile {|
(defn test []
  [:column {:text "/"}])
|} in
  Alcotest.(check string)
    "generated js"
    {|import { list, vector_QMARK_, concat, hash_map, truthy, not, print_result, println, eprintln, str, _EQ_, _PLUS_, _GT_, _LT_, _GT__EQ_, _LT__EQ_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "./language_runtime.js";
export const test = (() => {
return (list)("column", (hash_map)("text", "/"));
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
    {|import { list, vector_QMARK_, concat, hash_map, truthy, not, print_result, println, eprintln, str, _EQ_, _PLUS_, _GT_, _LT_, _GT__EQ_, _LT__EQ_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "./language_runtime.js";
export const handle_fetch = ((request, env, ctx) => {
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
    {|import { list, vector_QMARK_, concat, hash_map, truthy, not, print_result, println, eprintln, str, _EQ_, _PLUS_, _GT_, _LT_, _GT__EQ_, _LT__EQ_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "./language_runtime.js";
export const test = ((value) => {
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
    {|import { list, vector_QMARK_, concat, hash_map, truthy, not, print_result, println, eprintln, str, _EQ_, _PLUS_, _GT_, _LT_, _GT__EQ_, _LT__EQ_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "./language_runtime.js";
export const test = ((title) => {
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
    {|import { list, vector_QMARK_, concat, hash_map, truthy, not, print_result, println, eprintln, str, _EQ_, _PLUS_, _GT_, _LT_, _GT__EQ_, _LT__EQ_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "./language_runtime.js";
export const test = ((value) => {
return new Widget(value.toString());
});|}
    js

let cast_is_no_op () =
  let js = compile "(defn test [value] (cast java.util.List value))" in
  Alcotest.(check string)
    "generated js"
    {|import { list, vector_QMARK_, concat, hash_map, truthy, not, print_result, println, eprintln, str, _EQ_, _PLUS_, _GT_, _LT_, _GT__EQ_, _LT__EQ_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "./language_runtime.js";
export const test = ((value) => {
return value;
});|}
    js

let definition_visibility () =
  let js = compile {|
(defn public-f [] 1)
(defn- private-f [] 2)
(def public-value 3)
(def- private-value 4)
|} in
  Alcotest.(check string)
    "generated js"
    {|import { list, vector_QMARK_, concat, hash_map, truthy, not, print_result, println, eprintln, str, _EQ_, _PLUS_, _GT_, _LT_, _GT__EQ_, _LT__EQ_, _MINUS_, _STAR_, _SLASH_, count, get, map, reduce, drop } from "./language_runtime.js";
export const public_f = (() => {
return 1;
});
const private_f = (() => {
return 2;
});
export const public_value = 3;
const private_value = 4;|}
    js

let () =
  Alcotest.run "JS ns"
    [
      ( "compiler/ns",
        [
          Alcotest.test_case "require imports" `Quick require_imports;
          Alcotest.test_case "string require import" `Quick string_require_import;
          Alcotest.test_case "root namespace imports" `Quick root_namespace_imports;
          Alcotest.test_case "nested namespace imports" `Quick nested_namespace_imports;
          Alcotest.test_case "string literal with slash" `Quick string_literal_with_slash;
          Alcotest.test_case "default export" `Quick default_export;
          Alcotest.test_case "instance method call" `Quick instance_method_call;
          Alcotest.test_case "constructor call" `Quick constructor_call;
          Alcotest.test_case "constructor call with nested arg" `Quick constructor_call_with_nested_arg;
          Alcotest.test_case "cast is no-op" `Quick cast_is_no_op;
          Alcotest.test_case "definition visibility" `Quick definition_visibility;
        ] );
    ]
