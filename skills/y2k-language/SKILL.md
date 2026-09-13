---
name: y2k-language
description: Use when writing or fixing Y2K Language applications, compiling .clj source with ly2k, or checking supported language forms and standard library functions for eval, JavaScript, and Java targets.
---

# Y2K Language applications

Use this reference when writing application code. Y2K Language is an experimental Lisp-inspired language, **not a Clojure implementation**. A `.clj` extension alone does not imply Y2K Language: check the project for `ly2k` usage.

- Determine the target (`eval`, `js`, or `java`) from the application's configuration and build commands. Ask if the project does not establish it.
- Follow the application's existing build, run, and test commands. `ly2k` is installed on `$PATH`; the build system supplies runtime files.
- Use only the forms and functions supported below or explicitly provided by the application/dependencies. Do not invent Clojure APIs or reader syntax.
- This reference tracks the current compiler. Keep installed `ly2k` in step with it; older versions may lack features.

<!-- ponytail: one current-compiler reference; split by version only when applications need pinned older compilers. -->

## Common language

### Values and reader syntax

| Syntax | Meaning and limits |
| --- | --- |
| `nil`, `true`, `false` | Nil and boolean literals. |
| `42`, `-7` | Use decimal integers for portable arithmetic. Keep operands and intermediate results within signed 32-bit range for Java; wider numbers and floating-point arithmetic are not portable. |
| `"text"` | Strings, including Unicode. Decode `\"`, `\\`, `\n`, `\t`, `\r` once. Other escapes, including `\b`, `\f`, `\/`, `\q`, `\u0041`, remain literal backslash sequences. |
| `:name` | Becomes the string `"name"`; there is no separate keyword runtime type. |
| `[a b]` | Eager list construction, equivalent to `(list a b)`. Vectors and lists share one runtime representation. |
| `{:name value}` | Hash-map construction, equivalent to `(hash-map "name" value)`. Use distinct string/keyword keys for portability. |
| `(f arg...)` | Function call; `f` can also be an expression returning a function. |
| `'value`, `(quote value)` | Quoted atoms become textual values; quoted lists become lists of quoted values. Quoted numbers are text on JS/Java, not numeric literals. Macro expansion still traverses quoted content: do not assume full Clojure quoting of vectors, maps, or macro forms. |
| `; comment` | Comment through end of line. Separate forms with whitespace; commas are not whitespace. |
| `^TYPE form` | Type metadata on the next form; see Java-specific effects below. |

**Truthiness:** `false` and `nil` are falsey; **the strings `"false"` and `"nil"` are also falsey in the current runtimes**. `0`, `""`, and empty collections are truthy. JS host `undefined` is falsey too. Do not apply JavaScript or Clojure truthiness rules instead.

### Definitions, bindings, and control flow

In signatures, `...` denotes repetition, not source syntax.

| Form | Behavior |
| --- | --- |
| `(def name value)` | Top-level definition. **No nested `def`**, including inside `let`, `fn`, or `do`. |
| `(defn name [params...] body...)` | Top-level function definition. Use fixed positional parameters and a nonempty body; no multi-arity clauses, variadic `&`, or docstring syntax. |
| `(def- name value)`, `(defn- name [params...] body...)` | Private-marked definitions. JS suppresses exports; this is not access control across all targets. |
| `(fn [params...] body...)` | Lexical closure; captures locals. Parameter patterns are supported. Use arities 0–4 for Java. Prefer `(fn [x] (builtin x))` to passing bare builtin names as callbacks: Java runtime methods are not general function values. |
| `(let [pattern value ...] body...)` | Sequential bindings: later values see earlier bindings. Each destructured RHS is evaluated once. Returns the last body value. |
| `(do expr...)` | Executes in order and returns the last value. Use a nonempty body for portable results; empty bodies differ across targets. |
| `(if condition then else)` | Evaluates only the selected branch; `else` may be omitted, giving `nil`. |
| `(and expr...)` | Short-circuits at the first falsey value; otherwise returns the last. Empty form returns `true`. |
| `(or expr...)` | Returns the first truthy value; otherwise the last falsey value. Empty form returns `nil`. |
| `(cond test result ...)` | First truthy test wins. Conditions and selected result are evaluated only as needed. No match or no clauses gives `nil`. Requires complete pairs even after an always-true clause. `:else` is an ordinary truthy string at its position. |
| `(case value match result ... fallback)` | Evaluates `value` once, compares matches sequentially with binary `=`, selects the first equal match. Final unpaired form is fallback; no fallback and no match gives `nil`. Match forms are expressions, not Clojure grouped constants. |
| `(if-let [name expr ...] then else)` | Nonempty vector of symbol/value pairs, checked left to right. Stops on the first falsey value. Later bindings see earlier ones; `then` sees all. Optional `else` defaults to `nil`; do not rely on bindings being available in `else`. No destructuring here. |
| `(-> value step...)` | Inserts value as the first argument of each step; a bare function name is a step too. |
| `(->> value step...)` | Inserts value as the last argument of each step. |
| `(:key collection)` | Exactly one collection argument: equivalent to `(get collection "key")`. No default-value overload. |
| `(cast TYPE value)` | Evaluates value once. Transparent in `eval` and JS; a Java cast in Java. |
| `(ns app.main (:require [dep.core :as dep]))` | Namespace declaration and alias. Use one namespace at the beginning of a compiled file. Each `:require` clause accepts **one** vector; repeat the clause for multiple dependencies. Use `dep/function` for a qualified call. Resolution/loading is target-specific. |

User forms are expanded internally; do not write compiler-generated `compiler/*`, `let*`, `fn*`, or `set!` as an application API. Use Atom for mutable state. Reader shorthand `@cell`, `#(...)`, sets, syntax-quote, custom `defmacro`, `loop`/`recur`, and Clojure destructuring directives are not supported application syntax.

### Destructuring

- Sequential: `[first second]`; extra items are ignored, missing leaf values become `nil`.
- Associative: `{:name name :age age}`. Reversed keyword pairs `{name :name age :age}` also work in `let` and function parameter patterns.
- Nest both patterns. Each function parameter pattern consumes one argument.
- Reversed pairs are normalized only in binding patterns, not ordinary map expressions.
- Use collections of the expected shape. Eval rejects a list pattern applied to a non-list or a map pattern applied to a non-map, including missing nested collections; compiled destructuring follows `get`. Missing leaves and missing nested collections are different cases.
- No `:keys`, `:as`, `:or`, or rest-binding syntax. Prefer distinct local names: compiler lowering does not reliably preserve nested shadowing.

```clojure
(defn describe [{:name name :tags [tag]}]
  (str name "-" tag))

(defn test []
  (str (describe {:name "Ada" :tags ["first" "second"]})
       "-" ((fn [[left]] left) ["pair" "ignored"])))
;; test returns "Ada-first-pair"
```

## Cross-platform standard library

These 26 functions are available without imports on all three targets. Signatures show portable arities; host language permissiveness is not an extra overload.

| Call | Contract |
| --- | --- |
| `(list values...)` | Eager list, any arity; zero gives an empty list. |
| `(vector? value)` | True for the shared list/vector representation, false otherwise. |
| `(concat lists...)` | Concatenates lists in order; zero gives an empty list. Does not concatenate strings or maps. |
| `(hash-map key value ...)` | Any even number of arguments, including zero; odd count errors. Prefer unique string/keyword keys. |
| `(count collection)` | Number of list items or map entries; not a string-length function. |
| `(get collection key)` | Map lookup or list lookup by nonnegative integer index. Missing key/index or `nil` collection gives `nil`; preserves `false`. JS normalizes retrieved `undefined` to `nil`. No third default argument. |
| `(get-in collection keys)` | Repeated `get` using a vector path, including mixed keys/indices and a runtime-computed path. Empty path returns collection unchanged. Missing intermediate data gives `nil`; `false` at the end is preserved. Scalar traversal and a non-vector path error. No third default argument. |
| `(map f items)` | Eager unary mapping over one list. No multiple-collection or transducer overload. |
| `(reduce f collection)` | Left fold with first element as initial accumulator; empty collection errors. |
| `(reduce f init collection)` | Left fold over every element; empty collection returns `init`. Both forms accept a list or map; map items are `[key value]` pairs. Do not rely on portable map iteration order. Callback takes accumulator and item. |
| `(drop n items)` | List without first `n` items. Integer `n <= 0` keeps all; `n >= count` gives empty list. |
| `(str values...)` | Concatenates textual values without separators; zero gives `""`. Nil is `"nil"`, lists use parentheses, maps braces, functions `"#<function>"`, atoms `"#<atom>"`. Not a serialization format. |
| `(+ numbers...)` | Integer sum; zero arguments gives `0`. |
| `(- n numbers...)` | Subtract remaining integers from `n`; at least one argument. **Unary form returns `n`, not its negation.** |
| `(* numbers...)` | Integer product; zero arguments gives `1`. |
| `(/ n numbers...)` | Divide left to right, truncating toward zero at each step; at least one argument. **Unary form returns `n`, not its reciprocal.** Zero-divisor behavior is not portable. |
| `(= a b)` | Portable equality for same-type nil, booleans, strings, and integers. Use exactly two arguments; collection, mixed-type, function, and Atom comparisons are not portable. |
| `(not= a b)` | Negates binary `=` with the same portability limits. |
| `(not value)` | Boolean negation of language truthiness, including textual `"false"`/`"nil"`. |
| `(> a b)`, `(< a b)`, `(>= a b)`, `(<= a b)` | Exactly two integers. No chained comparisons. |
| `(atom value)` | New mutable reference; can hold nil, booleans, collections, and functions without calling them. |
| `(deref reference)` | Reads an Atom. Use this instead of `@reference`. |
| `(reset! reference value)` | Stores and returns value. Aliases share the same reference; separately created atoms are independent. |
| `(swap! reference f)` | Calls unary `f` once with current value, stores and returns its result. No extra arguments. If callback fails, no result is written; callback side effects are not rolled back. Sequential semantics only, no host-thread synchronization guarantee. |

Atom functions enforce arities 1, 1, 2, 2; invalid references or a non-callable update fail. Other APIs may report errors at compilation or execution depending on target.

**Representation differences:** eval stores scalar text and compares lists/maps structurally (map pair order matters); JS uses identity equality for collections, Java uses host `equals`. JS/Java maps stringify keys and overwrite duplicate keys, whereas eval retains pairs and looks up the first matching key. JS may reorder integer-like keys. Avoid duplicate/non-string keys, collection equality, and ordering assumptions in portable code.

```clojure
(defn test []
  (let [cell (atom 10)
        alias cell
        replaced (reset! alias 20)
        delta 3
        updated (swap! cell (fn [old] (+ old delta)))]
    (str replaced " " updated " " (deref alias))))
;; test returns "20 23 23"
```

```clojure
(defn test []
  (reduce (fn [acc [k v]] (str acc k v)) "" {:a 1 :b 2}))
;; test returns "a1b2" for these distinct non-integer-like keys
```

## Target-specific features

### eval

- Executes forms in order; globals are isolated by namespace (default `user`). Functions retain their definition namespace and lexical locals. `alias/member` resolves a declared alias; `namespace/member` can resolve directly.
- `ns` with `:require` establishes aliases, **not automatic file loading**. Java `:import` clauses have no host-interop effect here.
- `(deps {:package "version"})` loads package `.clj` files from `$LY2K_PACKAGES_DIR/PACKAGE/VERSION` at that point in execution. Put it before dependent references. Nested dependencies load too; each loaded file restores the caller's namespace afterward. Do not depend on directory enumeration order or assume this downloads packages.
- CLI output is only the final scalar text. A final list, map, function, or Atom produces an empty result string; use `str` to inspect it. Defining `test` does not call it: append `(test)` when evaluating such examples.
- `cast` ignores its type and returns the value; Java/JS host constructors and methods are not interpreted.
- Eval-only library functions such as `slurp` and `assert` are outside the cross-platform library above. Do not assume they exist in compiled applications.

### JavaScript (`js`)

- Generates an ES module, not executed output. Public definitions become `export const`; `def-`/`defn-` become non-exported `const`. Hyphens in identifiers become underscores; punctuation is munged (for example `!` becomes `_BANG_`, `?` becomes `_QMARK_`).
- Symbolic namespace requires become relative `.js` imports. Output layout follows namespace segments: `app.commands.add` corresponds to `app/commands/add.js`. Runtime import is relative to the output root (`../../language_runtime.js` in this example). Required namespace names are munged before dots become `/`.
- String requires preserve their decoded ESM module specifier: `(:require ["node:test" :as t])`, for example. No extra `.js` or namespace prefix is added; aliases are munged. Repeat `:require` clauses for multiple modules. Java `:import` clauses do not import JavaScript modules.
- Host calls: `(new Constructor args...)` or `(Constructor. args...)`; `(. receiver method args...)` or `(.method receiver args...)`; `(alias/function args...)` for a module function. Host APIs must exist in the actual application environment. No async/await language form is provided; use host callbacks/promises where appropriate.
- `(cast TYPE value)` is transparent; Java type metadata does not provide runtime checking.
- `(export-default :key value "other-key" other-value)` is a **top-level** ESM default export of an ordinary JS object. It takes one or more key/value pairs with literal keyword/string keys, not a map argument. Empty forms, odd argument counts, or computed keys fail. Ordinary map literals still use the language's null-prototype map representation.

```clojure
(defn handle-fetch [request env ctx]
  (Response. "OK"))

(export-default :fetch handle-fetch)
```

The example requires a host with `Response`, such as a current Node or a Fetch-compatible worker. The build system places the module and its runtime; use the application's runner to invoke the exported handler.

### Java (`java`)

- Generates a public helper class with static definitions and the language runtime import. No `ns` gives class `user`; `(ns app.main)` gives package `app`, class `main`, so the generated source filename must be `main.java`. Use the build system's Java entry point; no `main` method is generated automatically.
- Source must contain top-level definitions, optional initial `ns`, and optional `gen-class`. Put executable expressions inside functions, not at Java file top level.
- Top-level functions and ordinary language function values support arities **0–4**. Function results can be called directly, including factory results, values from `get`, and selected lambdas. A Java host interface with a method named `call` is not automatically a language function: use `(.call receiver ...)` for that host method.
- Functions are generated as static methods with `Object` parameters/results and may throw exceptions. Pass explicit lambdas for builtin callbacks. Top-level function references currently use helper name `user`; in named namespaces prefer an explicit lambda calling the function rather than a bare method value. `def-`/`defn-` do not add distinct Java private access modifiers.
- `(:require [other.core :as other])` maps qualified calls to the other helper class; it does not load source. Generated members are package-private, so cross-package calls may need an application-provided Java boundary; do not assume arbitrary namespace requires are Java-accessible.
- `(:import [java.time LocalDate] [java.util UUID])` imports classes; multiple class names per vector and multiple vectors per clause are supported.
- `(Class/staticMethod args...)` calls a static method; `(new Class args...)` and `(Class. args...)` call constructors; `(. value method args...)` and `(.method value args...)` call instance methods. Use actual accessible host constructors and signatures.
- `(cast TYPE value)` emits a Java cast. Use it to call methods on `Object` results or supply primitive host arguments. `^TYPE` on a symbol in `let` casts its RHS before binding; on a symbol parameter of `fn`/`defn`/`defn-` it introduces a local cast binding. These rules do not provide type inference or typed destructuring.
- `(instance? TYPE value)` is a Java-only special form, not a first-class function. `TYPE` is a static class/interface symbol, imported or fully qualified (for example, `(instance? java.util.List xs)`). It emits native `instanceof`, evaluates `value` exactly once even when its result is discarded, and returns a boolean: subclasses/interface implementations match; `nil` and unrelated objects do not. Primitive values are boxed (`(instance? Integer 42)` is true). It does not narrow the value's static type; use `cast` or a type hint for subsequent interop. Invalid arity, literal/expression type operands, and primitive type names are rejected during generation; unknown or inaccessible classes fail in `javac`. Dynamic Class values, parameterized types, and special array-type syntax are outside the contract. JS compilation and eval execution explicitly reject this form; eval does not evaluate its arguments.
- Annotate a lambda with `^java.util.function.Function` (or another compatible value-returning interface) for Java callbacks. Use `^void:java.lang.Runnable` or `^void:java.util.function.Consumer` for void-returning interfaces. The `void:` prefix requires a nonempty target type. Exceptions are rethrown through the runtime bridge.

```clojure
(defn test []
  (.orElse
   (.map
    (java.util.Optional/of "X")
    ^java.util.function.Function
    (fn [value] (str value "!")))
   "missing"))
;; test returns "X!" on Java
```

**`gen-class`:** `(gen-class :name NAME :extends BASE :methods [[method [ARG-TYPES...] void] ...])` emits a public static nested subclass inside the helper. Define each implementation as top-level `(defn -method [this args...] body...)`. Only `void` overrides are supported. `^override` on the declared method calls `super.method(...)` before the helper implementation. The total implementation arity, including `this`, must fit Java's 0–4 limit. A qualified `:name` must match the namespace/package rules; prefer one declaration with a simple nested class name per source file. It generates a nested class, not a separate top-level source file.

```clojure
(gen-class
 :name GeneratedThread
 :extends java.lang.Thread
 :methods [[^override run [] void]])

(defn -run [this]
  nil)

(defn test []
  (let [t (GeneratedThread.)]
    (.interrupt t)
    (.isAlive t)))
;; test returns false on Java
```

## CLI and project integration

`ly2k` reads source from **stdin**. It accepts `--target eval`, `--target js`, or `--target java`; default is `eval`. A source filename is not a positional argument.

```sh
ly2k --target eval < program.clj
ly2k --target js < program.clj > program.js
ly2k --target java < program.clj > user.java
```

The Java command assumes source without `ns`; use the declared helper class filename otherwise. Evaluation prints the final scalar; compilation writes generated source to stdout. Reported parse/eval errors go to stderr with a nonzero exit status; other compilation failures can also terminate with an error. Inspect both the compiler and the host runtime/build diagnostics.

Runtime files (`language_runtime.js`, `language_runtime.java`) are **automatically supplied by the application's build system**. Do not add manual copying, compile the compiler from source, or assume its checkout is the application's working directory. Generating JS/Java source does not run it. Follow existing build/run commands for module layout, dependencies, host entry points, and tests.

For OpenCode installation, symlink the directory containing this `SKILL.md` into the application as `.opencode/skills/y2k-language`. No `opencode.json` edit is needed. Restart OpenCode after connecting or updating the skill so a new session loads it.

## Repository and issues

- Source and project documentation: https://github.com/y2k/language
- Report compiler/language issues: https://github.com/y2k/language/issues

For a useful bug report, include the target, minimal Y2K source, the exact command/build step, expected result, and actual output or error. Include host/tool versions and compiler revision when known. Reduce application dependencies where possible; distinguish a `ly2k` failure from generated JS/Java compilation or execution failure.
