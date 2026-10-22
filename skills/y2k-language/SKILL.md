---
name: y2k-language
description: Use when writing or fixing Y2K Language applications, compiling .clj source with ly2k, or checking supported language forms and standard library functions for eval, JavaScript, and Java targets.
---

# Y2K Language applications

## Repository and issues

- Source and project documentation: https://github.com/y2k/language
- Report compiler/language issues: https://github.com/y2k/language/issues
- Create issues describing only the problem; do not include proposed solutions or implementation plans.

For a useful bug report, include the target, minimal Y2K source, the exact command/build step, expected result, and actual output or error. Include host/tool versions and compiler revision when known. Reduce application dependencies where possible; distinguish a `ly2k` failure from generated JS/Java compilation or execution failure.

## Usage

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
| `nil`, `true`, `false` | Nil and boolean literals. Eval represents nil and booleans as distinct values, not the strings `"nil"`, `"true"`, or `"false"`. |
| `42`, `-7` | Decimal integers. Keep integer operands and intermediate results within signed 32-bit range for portability; wider integer arithmetic differs by target. |
| `0.2`, `-0.5` | Finite fractional numbers supported by `+`, `-`, `*`, including mixed integer/fractional arguments. Uses binary64 precision, not exact decimal arithmetic. Fractional division and ordering comparisons are not portable. |
| `"text"` | Strings, including Unicode. Decode `\"`, `\\`, `\n`, `\t`, `\r` once. Other escapes, including `\b`, `\f`, `\/`, `\q`, `\u0041`, remain literal backslash sequences. |
| `:name` | Becomes the string `"name"`; there is no separate keyword runtime type. |
| `[a b]` | Eager list construction, equivalent to `(list a b)`. Vectors and lists share one runtime representation. |
| `{:name value}` | Hash-map construction, equivalent to `(hash-map "name" value)`. Use distinct string/keyword keys for portability. |
| `(f arg...)` | Function call; `f` can also be an expression returning a function. |
| `'value`, `(quote value)` | Eval preserves literal types: `'42` is an integer, `'0.25` a binary64 number, `'false` a boolean, `'nil` nil, and `'"42"` a string. Other quoted atoms such as `'foo` are symbols, distinct from `"foo"`. JS/Java quote atoms as text, including numbers, nil, and booleans. Quoted lists recursively contain quoted values. Macro expansion still traverses quoted content: do not assume full Clojure quoting of vectors, maps, or macro forms. |
| `; comment` | Comment through end of line. Separate forms with whitespace; commas are not whitespace. |
| `^TYPE form` | Type metadata on the next form; see Java-specific effects below. |

**Truthiness:** only boolean `false` and `nil` are falsey. **The strings `"nil"` and `"false"` are truthy on all targets.** `0`, `""`, and empty collections are truthy. JS host `undefined` is falsey too. Do not apply JavaScript or Clojure truthiness rules instead.

In eval, `(= nil "nil")` is `false`, `(if "nil" 1 2)` is `1`, and `(str nil)` is the truthy string `"nil"`. Nil and text `"nil"` also remain distinct in collections and as map keys; `get`/`get-in` reject traversal through text `"nil"` as through other strings. Eval's CLI still outputs `nil` for a final nil value. Use the literal `nil` for missing data; quoted nil is not portable across targets.

Eval also distinguishes booleans from text: `(= false "false")` is `false`, `(= 'false false)` is `true`, and `(= 'false "false")` is `false`. Predicates and comparisons return actual booleans. Boolean and text keys remain distinct in eval maps and destructuring. `(str false)` returns the truthy string `"false"`, so `(if (str false) 1 2)` returns `1`. This changes older eval versions where text `"false"` was falsey; use `(= value "false")` when testing for that text.

### Definitions, bindings, and control flow

In signatures, `...` denotes repetition, not source syntax.

| Form | Behavior |
| --- | --- |
| `(def name value)` | Top-level definition. **No nested `def`**, including inside `let`, `fn`, or `do`. |
| `(defn name [params...] body...)` | Top-level function definition. Use fixed positional parameters and a nonempty body; no multi-arity clauses, variadic `&`, or docstring syntax. |
| `(def- name value)`, `(defn- name [params...] body...)` | Private-marked definitions. JS suppresses exports; Java emits private members accessible only within their namespace (including nested `gen-class` classes). This is not access control across all targets. |
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
| `(:key dic)` | Shorthand for `(get dic :key)` (also equivalent to `(get dic "key")`); use either form for map lookup. Exactly one collection argument; no default-value overload. |
| `(cast TYPE value)` | Evaluates value once. Transparent in `eval` and JS; a Java cast in Java. |
| `(ns app.main (:require [dep.core :as dep]))` | Namespace declaration and alias. Use one namespace at the beginning of a compiled file. Each `:require` clause accepts **one or more** vectors, e.g. `(:require [alpha.core :as a] [beta.core :as b])`; separate clauses are equivalent and preserve dependency order. Empty `(:require)` is an error. Use `dep/function` for a qualified call. Resolution/loading is target-specific. |

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

These functions are available without imports on all three targets. Signatures show portable arities; host language permissiveness is not an extra overload.

| Call | Contract |
| --- | --- |
| `(list values...)` | Eager list, any arity; zero gives an empty list. |
| `(vector? value)` | True for the shared list/vector representation, false otherwise. |
| `(string? value)` | Boolean true for runtime strings, including `""` and keywords; false for other types. Evaluates its one argument once, without converting it to text. |
| `(concat lists...)` | Concatenates lists in order; zero gives an empty list. Does not concatenate strings or maps. |
| `(conj items value)` | Exactly two arguments. Returns a new list/vector with one value appended at the end, preserving the input. Lists and vectors both append at the end. |
| `(hash-map key value ...)` | Any even number of arguments, including zero; odd count errors. Prefer unique string/keyword keys. |
| `(assoc map key value)` | Exactly 3 arguments. Adds/replaces one field in a new shallow map; preserves the input. |
| `(merge left right)` | Exactly 2 maps. New shallow map with right-hand fields winning conflicts; preserves both inputs. |
| `(count collection)` | Number of list items or map entries; not a string-length function. |
| `(get collection key)` | Map lookup or list lookup by nonnegative integer index. Missing key/index or `nil` collection gives `nil`; preserves `false`. JS normalizes retrieved `undefined` to `nil`. No third default argument. |
| `(get-in collection keys)` | Repeated `get` using a vector path, including mixed keys/indices and a runtime-computed path. Empty path returns collection unchanged. Missing intermediate data gives `nil`; `false` at the end is preserved. Scalar traversal and a non-vector path error. No third default argument. |
| `(map f items)` | Eager unary mapping over one list. No multiple-collection or transducer overload. |
| `(filter pred items)` | Keeps original items whose unary predicate result is language-truthy: only nil/false reject an item (also undefined on JS). Zero, empty string, and text `"nil"`/`"false"` are truthy. |
| `(keep f items)` | Maps each item and keeps results other than nil (also excludes undefined on JS). Preserves false, 0, empty string, and text `"nil"`. |
| `(sort-by keyfn coll)` | Stable ascending sort by computed keys; returns a new list/vector without changing the input or cloning its elements. Exactly two arguments. |
| `(run! f items)` | Eager side-effect traversal of one list/vector in order, calling unary `f` once per item. Discards callback results without collecting them; returns actual `nil`, including for an empty collection (no callback calls). Callback errors stop traversal and propagate; earlier effects are not rolled back. |
| `(reduce f collection)` | Left fold with first element as initial accumulator; empty collection errors. |
| `(reduce f init collection)` | Left fold over every element; empty collection returns `init`. Both forms accept a list or map; map items are `[key value]` pairs. Do not rely on portable map iteration order. Callback takes accumulator and item. |
| `(drop n items)` | List without first `n` items. Integer `n <= 0` keeps all; `n >= count` gives empty list. |
| `(str values...)` | Concatenates textual values without separators; zero gives `""`. Nil is `"nil"`, lists use parentheses, maps braces, functions `"#<function>"`, atoms `"#<atom>"`. Not a serialization format. |
| `(+ numbers...)` | Sum integers and/or fractions left to right; zero arguments gives `0`. |
| `(- n numbers...)` | Subtract remaining integers and/or fractions from `n` left to right; at least one argument. **Unary form returns the value of `n`, not its negation.** |
| `(* numbers...)` | Multiply integers and/or fractions left to right; zero arguments gives `1`. |
| `(/ n numbers...)` | Portable for integers only. Divide left to right, truncating toward zero at each step; at least one argument. **Unary form returns `n`, not its reciprocal.** Zero-divisor behavior is not portable. |
| `(= a b)` | Portable equality for same-type nil, booleans, strings, and integers. Use exactly two arguments; collection, mixed-type, function, and Atom comparisons are not portable. |
| `(not= a b)` | Negates binary `=` with the same portability limits. |
| `(not value)` | Boolean negation of language truthiness. Text `"nil"` and `"false"` both give `false` on all targets. |
| `(> a b)`, `(< a b)`, `(>= a b)`, `(<= a b)` | Exactly two integers. No chained comparisons. |
| `(atom value)` | New mutable reference; can hold nil, booleans, collections, and functions without calling them. |
| `(deref reference)` | Reads an Atom. Use this instead of `@reference`. |
| `(reset! reference value)` | Stores and returns value. Aliases share the same reference; separately created atoms are independent. |
| `(swap! reference f)` | Calls unary `f` once with current value, stores and returns its result. No extra arguments. If callback fails, no result is written; callback side effects are not rolled back. Sequential semantics only, no host-thread synchronization guarantee. |

Atom functions enforce arities 1, 1, 2, 2; invalid references or a non-callable update fail. Other APIs may report errors at compilation or execution depending on target.

**Appending:** `conj` copies the collection shallowly, preserving element references and values such as nil and false. A collection-valued second argument stays one nested element: `(conj [1] [2 3])` gives `[1 [2 3]]`. Use a list/vector (also a dense host array on JS); maps, nil collections, sparse arrays, arbitrary iterables, and other arities are outside the portable contract. Eval rejects incorrect arities and collection types. Repeated appends copy growing collections and take quadratic total time. Deploy the compiler with its matching runtime exporting `conj`.

**Map updates:** `assoc` and `merge` require maps: nil, lists/vectors, and scalars in map positions error. Values such as nil, false, 0, and `""` are retained and overwrite previous values. Nested maps are replaced whole; nested objects and Atoms are shared, not cloned. Use unique string/keyword keys for portability, and check fields with `get` rather than relying on map equality or iteration order. Extra pairs/maps are not overloads; use nested calls:

```clojure
(let [item {:reason "selected" :status "pending"}
      result {:status "ready"}]
  (assoc (merge item result) :selection_reason (:reason item)))
```

On eval, updates use `=` for keys. Assoc replaces all equal-key pairs with one pair; merge applies right-hand pairs left to right, so the last duplicate on the right wins. Existing `hash-map` construction and first-match `get` stay unchanged. JS/Java use their existing stringified keys. JS also accepts ordinary JSON objects, copies own enumerable string fields, and returns a null-prototype object where `__proto__` and `constructor` are ordinary data keys. Class instances, getters, proxies, and symbol keys are outside this contract. Deploy generated code with the matching runtime exporting both functions.

**String checks:** `"nil"`, `"false"`, `"42"`, and `:name` are strings; nil, booleans, numbers, collections, functions, atoms, and regex values are not. Use `(string? value)` before string operations on external data. JS accepts primitive strings (including JSON.parse string results), not `new String("x")`, objects, or undefined. Java accepts `java.lang.String`, not `StringBuilder`. Quoted `'foo`, `'nil`, `'false`, and `'42` give false on eval but true on JS/Java, which represent quoted atoms as text; `'"text"` gives true everywhere. Use exactly one argument; incorrect arities on compiled targets are outside the portable contract.

`filter` and `keep` are ordinary two-argument functions over one list/vector. Both run eagerly, calling a named function, lambda, or local function value once per item in order, with only that item as its argument. They preserve result order and do not mutate the input collection or copy its element objects. Empty input gives an empty list without callback calls; rejecting every item also gives an empty list, not nil. Callback errors stop traversal and propagate; prior effects remain. Maps, nil collections, arbitrary iterables, transducers, async waiting, and collection mutation during traversal are outside the contract. JS supports ordinary dense host arrays; sparse arrays are outside the contract. JS/Java do not promise eval's argument diagnostics.

```clojure
(filter (fn [message] (= (:type message) "text")) messages)
(keep (fn [event] (if (:seen event) nil {:card (:id event)})) events)
```

`run!` is an ordinary function, not a macro. Pass a named function, lambda, or local function value. Its collection contract excludes maps, `nil`, and arbitrary iterables. On JS, ordinary dense host arrays are supported without changing the element objects; convert a DOM `NodeList` with `Array.from` first. Direct `NodeList` traversal, mutation of the collection during traversal, and awaiting async callbacks are not part of the contract. JS/Java do not promise eval's argument diagnostics.

**Sorting:** `sort-by` calls a named, lambda, or local unary `keyfn` once per element, eagerly from left to right, before sorting. Equal keys keep their original order. Empty input returns an empty list/vector without calling `keyfn`; singleton input calls it once. Callback errors stop traversal and propagate, retaining earlier effects. Unlike Clojure `sort-by`, keys are computed only once and there is no comparator overload.

Use either all string keys or all numeric keys in one call. Numeric keys can mix integers and finite binary64 values: order is numeric, without epsilon, and `1`/`1.0` and both signs of zero are ties. Eval preserves the order of large integers against binary64 without rounding the integer; cross-target portability remains limited by each target's numeric range. String keys (including keywords) use case-sensitive lexicographic order without locale: ASCII sorts identically everywhere; other Unicode is byte-ordered on eval and UTF-16-ordered on JS/Java. Nil, booleans, composite keys, mixed strings/numbers, and non-finite keys are outside the contract.

Input is a list/vector (also a dense host array on JS). Maps, nil collections, arbitrary iterables, sparse arrays, and callback mutation of the input or its elements are outside the contract. Elements themselves can be any values if their computed keys are supported. Deploy with the matching runtime exporting `sort_by`.

```clojure
(sort-by (fn [event] (:priority event))
         [{:id "A" :priority 20} {:id "B" :priority 3} {:id "C" :priority 20}])
;; Returns the original events in order B, A, C.
```

**Fractional arithmetic:** `+`, `-`, `*` preserve fractional operands without truncation. A result that is exactly an integer within signed 32-bit range is normalized to an integer, including negative zero becoming `0`. Thus `(str (+ 0.5 0.5))` is `"1"`, `(= (+ 0.5 0.5) 1)` is `true`, and the corresponding `not=` is `false`. This does not establish general mixed-literal equality such as `(= 1 1.0)`. Non-integer results retain binary64 precision when reused in arithmetic; no epsilon or decimal rounding is applied. For example, `(str (+ 1 0.2) " " (* 2 0.2))` is `"1.2 0.4"`, but `(+ 0.1 0.2)` need not equal `0.3` exactly. Exponent spelling and formatting of arbitrary fractional values can differ by target; `str` is not a portable numeric serialization format. Non-finite values and overflow remain outside the portable contract.

Java runtime methods for `+`, `-`, `*` return `Number` (`Integer` or `Double`) rather than always `Integer`. Recompile Java consumers with the updated runtime and deploy it together with the compiler's matching runtime version.

**Representation differences:** eval distinguishes nil, booleans, strings, symbols, machine integers, and binary64 numbers. `(= 42 "42")` and `(= 'foo "foo")` are false. Eval compares integers and binary64 numbers by exact numeric value: `(= 1 1.0)` is true, without epsilon or rounding a large integer to binary64. This mixed equality is not a cross-target guarantee. Eval compares lists/maps recursively (map pair order matters); functions and Atom are unequal even to themselves. JS uses identity equality for collections, Java uses host `equals`.

Eval map lookup, `get-in`, and associative destructuring use the same equality as `=` and return the first matching pair. For `(hash-map 1 "first" 1.0 "second" "1" "text")`, both numeric keys retrieve `"first"`, the string key retrieves `"text"`, and `count` remains 3. Symbol and string keys are distinct; function/Atom keys never match, even using the same reference. JS/Java maps stringify keys and overwrite duplicates; JS may reorder integer-like keys. Avoid duplicate/non-string keys, collection equality, and ordering assumptions in portable code.

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
- `(deps {:package "version"})` loads package `.clj` files from `$LY2K_PACKAGES_DIR/PACKAGE/VERSION` at that point in execution. Both package and version must be strings; keywords remain strings, but numbers and quoted symbols are rejected. Put it before dependent references. Nested dependencies load too; each loaded file restores the caller's namespace afterward. Do not depend on directory enumeration order or assume this downloads packages.
- Eval arithmetic accepts numeric values, never numeric strings or symbols. `+`, `-`, `*` retain numbers between operations; `count` returns an integer. `/`, ordering, `drop`, and list indices require integer values, rejecting even `1.0`. Thus `(get [10 20] (+ 0.5 0.5))` returns `20`, but `(get [10 20] 1.0)` errors. Fractional division and ordering are not supported in eval.
- `(slurp "notes.txt")` requires one string path, relative to the process working directory when not absolute, and returns the full file contents as a string. Numbers and symbols such as `'file` are rejected. Use string literals for text that older eval code expressed as quoted symbols.
- CLI output is only the final scalar: strings and symbols print without quotes, numbers use `str` formatting, nil prints `nil`, and booleans print `true`/`false`. Original numeric spelling is not preserved: `1.00` prints `1`. A final list, map, function, or Atom produces an empty result string; use `str` to inspect it. `str` always returns a string and also renders typed values inside collections, for example `(str [false true])` returns `"(false true)"`. Defining `test` does not call it: append `(test)` when evaluating such examples.
- `cast` ignores its type and returns the value; Java/JS host constructors and methods are not interpreted.
- Eval-only library functions such as `slurp` and `assert` are outside the cross-platform library above. Do not assume they exist in compiled applications.
- Eval regex values are opaque and truthy. Bindings, functions, collections, destructuring, and Atom preserve them. Like functions/Atom, regex values are unequal even to themselves in binary equality, including nested collection comparisons; regex map keys never match. Unary `=`/`not=` still return true/false. `(str (re-pattern "x"))` returns `"#<regex>"`; a final regex produces an empty runner result, so use `str` to inspect it. Regex values are not callable.

#### Eval argument contracts

These are **eval-specific** arities; continue using binary `=`/`not=` in portable code. Arguments are evaluated eagerly before builtin validation. Wrong arity/type raises `Eval_error` naming the operation and expected arguments; there is no string/symbol-to-number coercion. “Integer” excludes binary64 literals such as `1.0`.

| Binding | Arity | Arguments and boundaries |
| --- | --- | --- |
| `list`, `str` | 0+ | Any values; results are list/string respectively. |
| `hash-map` | Even, including 0 | Any key/value pairs; retain order and duplicates. |
| `assoc` | 3 | Map, any key, any value. |
| `merge` | 2 | Two maps. |
| `concat` | 0+ | Lists only. |
| `conj` | 2 | List/vector and any value; appends one element at the end. |
| `=`, `not=` | 0+ | Any values; `=` tests whether all equal the first, `not=` negates that result. Zero/one argument gives `true`/`false` respectively. |
| `not`, `assert`, `vector?`, `string?` | 1 | Any value; `assert` returns true for truthy values, errors for nil/false. |
| `atom` | 1 | Any value. |
| `deref` | 1 | Atom. |
| `reset!` | 2 | Atom and any new value. |
| `swap!` | 2 | Atom and function. |
| `count` | 1 | List/map; returns integer size. |
| `slurp` | 1 | String path. |
| `re-pattern` | 1 or 2 | String pattern, optional string flags (`""` or `"i"`) → compiled regex. |
| `re-find` | 2 | Regex, string text → first full match string or nil. |
| `re-find-groups` | 2 | Regex, string text → list of full match and capture groups, or nil. |
| `re-replace` | 3 | String text, regex, string replacement → globally replaced string. |
| `get` | 2 | Map/any key; list/nonnegative integer index; nil/any key. |
| `get-in` | 2 | Any initial value and a list/vector path; each step follows `get`. |
| `map` | 2 | Function and list. |
| `filter`, `keep` | 2 | Function and list/vector; return a list. |
| `run!` | 2 | Function and list/vector; returns nil. |
| `reduce` | 2 or 3 | Function, optional initial value, list/map; without init the collection must be nonempty. |
| `drop` | 2 | Integer and list; count <= 0 keeps the list. |
| `+`, `*` | 0+ | Integer/binary64 numbers; empty calls return 0/1. |
| `-` | 1+ | Integer/binary64 numbers; unary preserves the argument's value, not its negation. |
| `/` | 1+ | Integers; divisors must be nonzero; unary returns the argument, including `(/ 0)` → 0. |
| `>`, `<`, `>=`, `<=` | 2 | Integers only. |

`map`, `filter`, `keep`, `run!`, both `reduce` forms, and `swap!` reject non-functions before iteration, including `(filter 42 [])`, `(keep 42 [])`, `(map 42 [])`, `(run! 42 [])`, and `(reduce 42 [1])`. A valid function is not called on an empty collection or a one-element reduce without init. Its parameter arity is checked only when called. Callback errors propagate unchanged; a failing `swap!` does not perform its final write, but callback effects (even an explicit `reset!` of the same Atom) remain.

`get`, `get-in`, and let/function associative patterns share the numeric/type-sensitive key equality described above. Missing data stays nil: `(get nil -1)` and `(get-in {} [:missing -1])` return nil. `get-in` validates the entire path's list/vector type even when starting from nil; an empty path returns any starting value unchanged. Once a step reaches a list, an invalid index errors.

Negative list indices raise `Eval_error`, even on empty lists. Every actual `/` step rejects a zero divisor, including `(/ 20 2 0)`; successful division truncates toward zero (`(/ -7 2)` → -3). Invalid file paths, including NUL, and file I/O failures produce `slurp failed: PATH`. Exact legacy errors remain `assertion failed`, `hash-map arguments must be key/value pairs`, and `slurp expects one path`; other diagnostic wording is not a stable format. Runner returns an error, and the CLI writes it to stderr with a nonzero exit status. These error guarantees are eval-specific, not promises for JS/Java.

#### Regular expressions on all targets

Regex functions are available without imports on eval, JavaScript, and Java: `(re-pattern pattern)` or `(re-pattern pattern flags)`, `(re-find regex text)`, `(re-find-groups regex text)`, and `(re-replace text regex replacement)`. JavaScript uses native `RegExp`; Java uses `java.util.regex.Pattern`.

Omitted flags or `""` select case-sensitive matching; `"i"` ignores ASCII case in searches and replacements while preserving the original text in matches and groups. Other flags, including `"g"`, `"m"`, `"ii"`, and non-string values, are errors. Unicode case matching remains engine-specific.

Regex values are opaque, truthy, and not callable on all targets. Functions, bindings, collection values, destructuring, and Atom preserve them. `str` produces `#<regex>`, including inside lists/maps. Binary `=` returns false whenever either direct operand is a regex, even the same value; binary `not=` returns true. This changes previous JS/Java identity equality. Recursive collection equality, regex map keys, unary equality, and final runner output are **not** unified: the stronger eval guarantees above remain eval-only. Reusing a compiled regex after any search or replacement starts a fresh operation, unaffected by previous match positions.

Portable patterns and text use printable ASCII, TAB, and LF. The shared subset includes literals, escaped metacharacters, simple positive/negative classes and ranges, `\d`/`\D`/`\w`/`\W`/`\s`/`\S` inside and outside classes, `.`, concatenation, alternation, ordinary groups, greedy/lazy `*`/`+`/`?`/bounded quantifiers, and `^`/`$`. Dot excludes LF; anchors refer to the whole string, including a strict end (`foo$` does not match `"foo\n"`), also with `"i"`. Unicode and other control characters are target-specific. POSIX/nested/intersected classes, inline flags, lookaround, and backreferences are outside the portable guarantee; native JS/Java extensions need not be rejected just because eval rejects them. Regex reader syntax is not provided.

The project uses Re **1.13.2**, pinned to avoid a Re 1.14.0 regression in bracketed `\W`. Both `"\\W+"` and `"[\\W]+"` match non-word characters; neither matches `"abc_42"`.

```clojure
(re-find (re-pattern "foo([0-9]+)") "xfoo42 foo7") ; "foo42", not capture groups
(get (re-find-groups (re-pattern "post-([0-9]+)" "i") "POST-5657") 1) ; "5657"
(re-replace "foo1 foo22" (re-pattern "foo[0-9]+") "X") ; "X X"
(re-replace "export function f() {}" (re-pattern "^export ") "") ; "function f() {}"
```

`re-find` searches from the left. No match is actual falsey nil; an empty match is the truthy string `""`. Reuse a compiled regex through a binding. Text is never implicitly compiled. Each API enforces its arity and runtime types on all targets, even for empty input or absent matches. Keywords remain string values; numbers, booleans, nil, collections, and callbacks are not coerced to strings. Quoted symbols are distinct from strings on eval but may be runtime strings on compiled targets, so their rejection is not portable. Invalid patterns and argument errors name their builtin and the reason; exact messages and host exception types are not portable. Eval reports errors through its runner/CLI; an unhandled error in an executed Node/JVM program produces stderr and a nonzero exit status. The JS/Java compilation step does not execute regex calls or promise to diagnose invalid patterns.

`re-find-groups` uses the same first-match search as `re-find`, returning an ordinary list/vector: full match at index 0, then groups in opening-parenthesis order. Use `get`, `count`, or positional destructuring. A match without groups returns a one-element collection; no match returns falsey nil. An unmatched optional group or alternative retains a nil slot (actual null on JS/Java), while a participating empty group is the truthy string `""`. For example, `"(a)?(b)"` on `"b"` yields `["b" nil "b"]`, and `"(a*)b"` yields `["b" ""]`. Captures inside repeated groups follow the native engine and are not portable; repeating characters inside one capture, as in `"post-([0-9]+)"`, is portable. `re-find` always keeps its string-or-nil result.

The eval dialect is **OCaml Re.Perl**, using default options or `Caseless` for `"i"`: a Perl-style subset, not Java Pattern or full Perl/PCRE compatibility. It supports literals, character classes (including `\\d`, `\\s`, `\\S` in language string literals), alternation, groups, greedy/lazy quantifiers, and anchors. Matching is byte-based; Unicode text is not normalized and `.` can select one byte of a UTF-8 character. `.` excludes newline; `^`/`$` match the start/end of the whole string (`foo$` does not match `"foo\n"`). Eval rejects inline flags such as `(?m)`/`(?s)`, lookaround, and backreferences. For multiline extraction use `[\\s\\S]*?` in a language string. Regex literals `#"..."` are not supported.

On all targets, `re-replace` replaces all non-overlapping matches, without rescanning inserted text. Replacement is literal: `$1`, `$&`, and backslash sequences are not capture substitutions; callback replacement is unavailable. No match preserves the input, and an empty replacement deletes matches. Empty matches insert the replacement and advance over one original ASCII character, preserving it; an empty match immediately after a nonempty match at the same position is skipped. End-of-input is processed at most once, and anchors remain relative to the whole original text. For example, replacing `""` with `"-"` in `"ab"` gives `"-a-b-"`; replacing `"a*"` gives `"-b-"`. This changes the previous JS/Java native replacement behavior (`"--b-"`). Outside the portable subset, empty-match advancement uses a byte on eval and a UTF-16 code unit on JS/Java; Unicode matching remains engine-specific.

**Text-based userscript assembly:** for known generated inputs with a single initial runtime import and no `export ` inside JS strings, save this as `build.clj`:

```clojure
(defn assemble-text [header-source runtime program]
  (let [header (re-find (re-pattern "// ==UserScript==[\\s\\S]*?// ==/UserScript==") header-source)
        exports (re-pattern "export ")
        body (re-replace program (re-pattern "^import [^\n]*\n") "")]
    (assert header)
    (str header "\n" (re-replace runtime exports "") "\n"
         (re-replace body exports ""))))

(assemble-text (slurp "header.txt") (slurp "language_runtime.js") (slurp "compiled.js"))
```

Run `ly2k --target js < app.clj > compiled.js`, then `ly2k --target eval < build.clj > app.user.js` from the directory containing the three inputs. `header.txt` must include the `// ==UserScript==` and `// ==/UserScript==` delimiters. The compiler's JS runtime is `prelude/language_runtime.js`; place it beside these inputs as `language_runtime.js`. This is a text transformation for that input format, not a JavaScript parser or general bundler. Regex guarantees in this section apply to eval only.

### Raw host code (`js` and `java`)

`(raw-code "text")` is a **special form**, not a function or user macro. Use a single source string literal as its argument. Payload extraction rejects non-literal operands that remain after lowering; it does not guarantee rejection of expressions folded into literals. The parser decodes ordinary string escapes once; the compiler inserts that decoded text unchanged, without escaping, identifier munging, interpolation, parentheses, `return`, an extra `;`, or a wrapper. Supply any required host terminators yourself. An empty string inserts no code.

Use it directly at top level, or as a non-final statement in a function's sequential body (including `let`/`do`). Finish the body with an ordinary language expression such as `nil`. Expression-valued raw-code is unsupported: do not use it as a call argument, callee, binding/def RHS, condition, direct `if` branch result or final body form. For conditional statements inside a function, use `(if condition (do (raw-code "...") nil) nil)`. Quoted forms remain data. The compiler does not validate source positions before lowering or guarantee diagnostics for unsupported placements. Arity and literal checks happen when emitting the payload.

`raw-code` is reserved in call position, so a user function with that name cannot be called using this syntax. The payload is trusted host code, **not sandboxed** and not translated between targets. Host syntax, scope and type errors remain the host toolchain's responsibility. Host declarations are not registered in the compiler's function/local tables; use actual host identifiers, not assumed compiler temporary names.

Eval rejects a reached raw-code form as supported only on JS/Java, without evaluating its arguments (even malformed ones). Quoted raw-code remains data, and an unselected `if` branch is not reached: `(if false (raw-code "...") "ok")` returns `"ok"` on eval.

### JavaScript (`js`)

- Generates an ES module, not executed output. Public definitions become `export const`; `def-`/`defn-` become non-exported `const`. Hyphens in identifiers become underscores; punctuation is munged (for example `!` becomes `_BANG_`, `?` becomes `_QMARK_`).
- Symbolic namespace requires become relative `.js` imports. Output layout follows namespace segments: `app.commands.add` corresponds to `app/commands/add.js`. Runtime import is relative to the output root (`../../language_runtime.js` in this example). Required namespace names are munged before dots become `/`.
- String requires preserve their decoded ESM module specifier: `(:require ["node:path" :as path] ["node:url" :as url])`, for example. No extra `.js` or namespace prefix is added; aliases are munged. Java `:import` clauses do not import JavaScript modules.
- Host calls: `(new Constructor args...)` or `(Constructor. args...)`; `(. receiver method args...)` or `(.method receiver args...)`; `(alias/function args...)` for a module function. Host APIs must exist in the actual application environment. No async/await language form is provided; use host callbacks/promises where appropriate.
- `(cast TYPE value)` is transparent; Java type metadata does not provide runtime checking.
- `(export-default :key value "other-key" other-value)` is a **top-level** ESM default export of an ordinary JS object. It takes one or more key/value pairs with literal keyword/string keys, not a map argument. Empty forms, odd argument counts, or computed keys fail. Ordinary map literals still use the language's null-prototype map representation.
- Top-level `raw-code` inserts directly into the ES module, permitting host declarations/imports/exports. Within a function it inserts statements before the ordinary final expression:

```clojure
(raw-code "export const hostValue = 42;")
(defn test []
  (raw-code "console.log('hello');")
  (str hostValue))
```

```clojure
(defn handle-fetch [request env ctx]
  (Response. "OK"))

(export-default :fetch handle-fetch)
```

The example requires a host with `Response`, such as a current Node or a Fetch-compatible worker. The build system places the module and its runtime; use the application's runner to invoke the exported handler.

### Java (`java`)

- Top-level `raw-code` inserts **inside the generated helper class**, in source order. Write fields, methods, nested classes or explicit static blocks yourself; it does not automatically create a static initializer or move `import` outside the class. Within a function it inserts statements:

```clojure
(raw-code "public static int hostValue = 42;")
(defn test []
  (raw-code "System.out.println(\"hello\");")
  (str hostValue))
```

- Generates a public helper class with static definitions and the language runtime import. No `ns` gives class `user`; `(ns app.main)` gives package `app`, class `main`, so the generated source filename must be `main.java`. Use the build system's Java entry point; no `main` method is generated automatically.
- Source must contain top-level definitions, optional initial `ns`, and optional `gen-class`. Put executable expressions inside functions, not at Java file top level.
- Top-level functions and ordinary language function values support arities **0–4**. Function results can be called directly, including factory results, values from `get`, and selected lambdas. A Java host interface with a method named `call` is not automatically a language function: use `(.call receiver ...)` for that host method.
- Functions are generated as static methods with `Object` parameters/results and may throw exceptions. Ordinary top-level definitions become `public static` methods or fields; `def-`/`defn-` become `private static`. Private members remain accessible within their namespace, including nested `gen-class` classes, but access from another namespace is rejected by `javac`, even in the same Java package. Migrate external uses of private-marked definitions by making them public or exposing a public wrapper. Pass explicit lambdas for builtin callbacks. Top-level function references currently use helper name `user`; in named namespaces prefer an explicit lambda calling the function rather than a bare method value.
- `(:require [other.core :as other])` maps qualified calls and field reads (`other/value`) to the other helper class; it does not load source. Public members are accessible across Java packages when the generated sources and runtime are compiled together or provided on the classpath.
- `(:import [java.time LocalDate] [java.util UUID])` imports classes; multiple class names per vector and multiple vectors per clause are supported.
- `(Class/staticMethod args...)` calls a static method; `(new Class args...)` and `(Class. args...)` call constructors; `(. value method args...)` and `(.method value args...)` call instance methods. Use actual accessible host constructors and signatures.
- `(cast TYPE value)` emits a Java cast. Use it to call methods on `Object` results or supply primitive host arguments. `^TYPE` on a symbol in `let` casts its RHS before binding; on a symbol parameter of `fn`/`defn`/`defn-` it introduces a local cast binding. These rules do not provide type inference or typed destructuring.

```clojure
(defn make-text []
  "foo")

(defn test []
  (let [^java.lang.String text (make-text)]
    (.concat text "!")))
;; Returns "foo!" on Java.
```

Here `make-text` returns `Object`; `^java.lang.String` explicitly casts its result before binding `text`, allowing the `.concat` call.

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

**`gen-class`:** `(gen-class :name NAME :extends BASE :methods [[method [ARG-TYPES...] void] ...])` emits a public static nested subclass inside the helper. Define each implementation as top-level `(defn -method [this args...] body...)`. New and overriding public `void` instance methods are supported; static and non-void methods are not. Generated methods have no `@Override` annotation: overriding follows Java's signature and inheritance rules, with no separate check of overriding intent. Without metadata, the method calls only the helper implementation. `^override` on the declared method calls `super.method(...)` first, then the helper after normal completion; it requires an accessible concrete superclass method and is not needed to override without calling `super`. The total implementation arity, including `this`, must fit Java's 0–4 limit. A qualified `:name` must match the namespace/package rules; prefer one declaration with a simple nested class name per source file. It generates a nested class, not a separate top-level source file.

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

On JDK 25, declare an instance `main()` to launch the nested class directly, without a static-main wrapper or preview flags:

```clojure
(ns checks.entry)

(gen-class
 :name Runner
 :extends Object
 :methods [[main [] void]])

(defn -main [this]
  (println "ok"))
```

The generated source is `entry.java`. After the application's build compiles it together with the runtime into `out`, run `java -cp out 'checks.entry$Runner'` on JDK 25; it prints `ok`. Quote the binary name to protect `$` from shell expansion. JDK 25 is required for this direct instance entry point, not for ordinary new instance methods.

## CLI and project integration

`ly2k` reads source from **stdin**. It accepts `--target eval`, `--target js`, or `--target java`; default is `eval`. A source filename is not a positional argument.

```sh
ly2k --target eval < program.clj
ly2k --target js < program.clj > program.js
ly2k --target java < program.clj > user.java
```

The Java command assumes source without `ns`; use the declared helper class filename otherwise. Evaluation prints the final scalar; compilation writes generated source to stdout. Reported parse/eval errors go to stderr with a nonzero exit status; other compilation failures can also terminate with an error. Inspect both the compiler and the host runtime/build diagnostics.

Runtime files (`language_runtime.js`, `language_runtime.java`) are **automatically supplied by the application's build system**. Do not add manual copying, compile the compiler from source, or assume its checkout is the application's working directory. Generating JS/Java source does not run it. Follow existing build/run commands for module layout, dependencies, host entry points, and tests.

### Printing and diagnostics

`println` and `eprintln` are available without imports on eval, JS, and Java. They accept zero or more values, format each as `str` does, join them with one space, and add a newline. With no arguments they print a blank line. `println` writes to stdout and `eprintln` to stderr (JS uses `console.log` and `console.error`). Both return nil in eval, null in Java, and undefined in JS.

Eval CLI prints the final result separately: `(eprintln "hello")` writes `hello\n` to stderr and `nil\n` to stdout, with exit status 0. `(println "hello")` writes `hello\nnil\n` to stdout. For diagnostics alongside a useful result:

```clojure
(do (eprintln "processing") "result")
```

On eval this writes `processing\n` to stderr and `result\n` to stdout. Use `eprintln` to keep diagnostics out of redirected result output.

### JavaScript source maps

For Node error stacks, compile with `--source-map` and execute the saved ES module with source maps enabled.
For a source saved as `src/my demo.clj`, assuming the build supplies `dist/language_runtime.js`:

```sh
ly2k --target js --source-map --source-file "../src/my demo.clj" < "src/my demo.clj" > dist/demo.mjs
node --enable-source-maps dist/demo.mjs
```

The inline Source Map v3 embeds the exact stdin text. `--source-file PATH` sets its source name; relative paths
resolve from the generated JS directory, so the example points to `src/my demo.clj`. Pass literal filenames,
including spaces, `#`, `?`, and `%`; URL escaping is handled by the compiler. PATH is metadata only: the compiler
reads stdin, never PATH, and does not make it absolute. `--source-file` alone has no effect on output.
Without `--source-file`, the name is `user.clj`, resolved beside the generated JS; that file need not exist.
Frames point to the **start of the failing expression**, including nested calls, callbacks/Promise, threading
steps, and calls inside `let`, destructuring, `if`, and `do`. A failing call indented two spaces on line 3 maps
to `3:3` (`user.clj:3:3` by default); columns use UTF-16 units. An entire `raw-code` payload maps to the start
of its form, and runtime frames retain their JS positions. Maps do not restore frames omitted by Node.
The flag is JS-only; eval/Java reject it. Compilation without the flag preserves ordinary JS output.

For OpenCode installation, symlink the directory containing this `SKILL.md` into the application as `.opencode/skills/y2k-language`. No `opencode.json` edit is needed. Restart OpenCode after connecting or updating the skill so a new session loads it.
