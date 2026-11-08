---
name: y2k-language
description: Use when writing or fixing Y2K Language applications, compiling .clj source with ly2k, or checking supported forms and functions for eval, JavaScript, Java, and FEX targets.
---

# Y2K Language applications

## Usage and target selection

Use this reference when writing application code. Y2K Language is an experimental Lisp-inspired language, **not a Clojure implementation**. A `.clj` extension alone does not imply Y2K Language: check the project for `ly2k` usage.

Determine the target of the code involved in the current task from the application's configuration and build/run commands. Ask if the project does not establish it. `ly2k` without `--target` executes eval; JavaScript is not a universal default.

Read this common reference and the target reference for the task:

| Target | Required reference | Read when |
| --- | --- | --- |
| `js` | [JavaScript](references/js.md) | Writing, compiling, or debugging JavaScript-target code, its modules, host APIs, or source maps. |
| `eval` | [Eval](references/eval.md) | Working on code executed by eval, including an eval-run build script. |
| `java` | [Java](references/java.md) | Writing, compiling, or debugging Java-target code, host interop, or generated classes. |
| `fex` | [FEX](references/fex.md) | Compiling or unpacking portable definition bundles, checking FEX's supported subset, canonical keys, or RHS records. |

Resolve these paths relative to the directory containing this `SKILL.md`, including when that directory is symlinked into an application; they are not relative to the application's working directory. Load only the references for affected targets. For a JS application with an eval-run `build.clj`, add the eval reference when the task touches that script. A build change that uses no eval code needs no eval reference. For changes to shared compiler semantics, read the references for every affected target.

- Follow the application's existing build, run, and test commands. `ly2k` is installed on `$PATH`; the build system supplies runtime files.
- Use only the forms and functions documented here, in the selected target reference, or explicitly provided by the application/dependencies. Do not invent Clojure APIs or reader syntax.
- This reference tracks the current compiler. Keep installed `ly2k` in step with it; older versions may lack features.

## Repository and issues

- Source and project documentation: https://github.com/y2k/language
- Report compiler/language issues: https://github.com/y2k/language/issues
- Create issues describing only the problem; do not include proposed solutions or implementation plans.

For a useful bug report, include the target, minimal Y2K source, the exact command/build step, expected result, and actual output or error. Include host/tool versions and compiler revision when known. Reduce application dependencies where possible; distinguish a `ly2k` failure from generated JS/Java compilation or execution failure.

<!-- ponytail: one current-compiler reference; split by version only when applications need pinned older compilers. -->

## Common language

This common language and library reference describes the executable eval/JS/Java targets. For FEX, use the narrower source and builtin subset in [FEX](references/fex.md); its current compiler emits bundles without executing them.

### Values and reader syntax

| Syntax | Meaning and limits |
| --- | --- |
| `nil`, `true`, `false` | Nil and boolean literals, distinct from the strings `"nil"`, `"true"`, and `"false"`. |
| `42`, `-7` | Decimal integers. Keep integer operands and intermediate results within signed 32-bit range for portability; wider integer arithmetic differs by target. |
| `0.2`, `-0.5` | Finite fractional numbers supported by `+`, `-`, `*`, including mixed integer/fractional arguments. Uses binary64 precision, not exact decimal arithmetic. Fractional division and ordering comparisons are not portable. |
| `"text"` | Strings, including Unicode. Decode `\"`, `\\`, `\n`, `\t`, `\r` once. Other escapes, including `\b`, `\f`, `\/`, `\q`, `\u0041`, remain literal backslash sequences. |
| `:name` | Becomes the string `"name"`; there is no separate keyword runtime type. |
| `[a b]` | Eager list construction, equivalent to `(list a b)`. Vectors and lists share one runtime representation. |
| `{:name value}` | Hash-map construction, equivalent to `(hash-map "name" value)`. Use distinct string/keyword keys for portability. |
| `(f arg...)` | Function call; `f` can also be an expression returning a function. |
| `'value`, `(quote value)` | Eval preserves literal types and distinguishes symbols from strings; JS/Java quote atoms as text, including numbers, nil, and booleans. Use literal `nil` for portable missing data. Quoted lists recursively contain quoted values. Macro expansion still traverses quoted content: do not assume full Clojure quoting of vectors, maps, or macro forms. |
| `; comment` | Comment through end of line. Separate forms with whitespace; commas are not whitespace. |
| `^TYPE form` | Type metadata on the next form; Java effects are in its target reference. |

**Truthiness:** only boolean `false` and `nil` are falsey (also host `undefined` on JS). **The strings `"nil"` and `"false"` are truthy on all targets.** `0`, `""`, and empty collections are truthy. Do not apply JavaScript or Clojure truthiness rules instead. `str` returns text, so `(if (str false) 1 2)` returns `1`.

### Definitions, bindings, and control flow

In signatures, `...` denotes repetition, not source syntax.

| Form | Behavior |
| --- | --- |
| `(def name value)` | Top-level definition. **No nested `def`**, including inside `let`, `fn`, or `do`. |
| `(defn name [params...] body...)` | Top-level function definition. Use fixed positional parameters and a nonempty body; no multi-arity clauses, variadic `&`, or docstring syntax. |
| `(def- name value)`, `(defn- name [params...] body...)` | Private-marked definitions; visibility is target-specific, not access control across all targets. |
| `(fn [params...] body...)` | Lexical closure; captures locals. Parameter patterns are supported. Portable functions use arities 0–4 and explicit lambdas such as `(fn [x] (builtin x))` for builtin callbacks. Java-specific details apply when Java is an affected target. |
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

Target-only forms: `raw-code` is supported on JS/Java; `export-default` on JS; `instance?` and `gen-class` on Java. Their contracts and examples are in the target references selected above.

### Synchronous exceptions (JS/Java only)

```clojure
(try
  body...
  (catch error handler...)
  (finally cleanup...))
(throw error)
```

Body and each present section must be nonempty. Supply at least one section:
one `catch`, one `finally`, or `catch` followed by `finally`. Ordinary body
expressions precede all sections. The catch binding is a simple unqualified
symbol (not a literal or destructuring pattern), visible only in its handler.
Nested `let` shadowing stays local; the body, cleanup, and following expressions
retain their outer bindings. Closures can capture the caught value.

`try` returns the last body value on success or the last handler value on recovery,
preserving actual `nil` and `false`. It works in value and discarded positions.
Cleanup runs once on normal or exceptional exit, including a handler failure;
its value is discarded. A cleanup error replaces the previous result/error,
without adding a cause or suppressed exception. Earlier effects remain performed.

`throw` takes exactly one expression, evaluates it once, and throws the original
value: any JS value, or a Java `Throwable`. Java catches `Exception`, not `Error`.
Use host APIs for constructing, inspecting, and logging errors; rethrow with
`(throw error)` to retain identity. JS protection is synchronous and does not
await a returned Promise. See the target reference for host examples.

Eval and FEX do not support executable `try`/`throw`; quoted forms remain data.
These names are reserved in call position on compiled targets. Generated `try`
uses native statements at its execution site, including function bodies and
definition initializers, rather than a function wrapper for its result.

Protect each resource immediately after successfully opening it. Nest cleanup
so an inner close failure still runs the outer close; `with-open` is not provided.
With application-defined opening/processing/closing functions:

```clojure
(let [first (open-first)]
  (try
    (let [second (open-second)]
      (try (process first second)
           (finally (close-second second))))
    (finally (close-first first))))
```

An `open-second` failure closes only `first`. A processing failure skips later
success-only actions; place publication/finalization after successful processing.

### Destructuring

- Sequential: `[first second]`; extra items are ignored, missing leaf values become `nil`.
- Associative: `{:name name :age age}`. Reversed keyword pairs `{name :name age :age}` also work in `let` and function parameter patterns.
- Nest both patterns. Each function parameter pattern consumes one argument.
- Reversed pairs are normalized only in binding patterns, not ordinary map expressions.
- Use collections of the expected shape. Missing leaves and missing nested collections are different cases: eval checks pattern shapes; compiled destructuring follows `get`.
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

**Appending:** `conj` copies the collection shallowly, preserving element references and values such as nil and false. A collection-valued second argument stays one nested element: `(conj [1] [2 3])` gives `[1 [2 3]]`. Use a list/vector; maps, nil collections, arbitrary iterables, and other arities are outside the portable contract. Repeated appends copy growing collections and take quadratic total time. Deploy the compiler with its matching runtime exporting `conj`.

**Map updates:** `assoc` and `merge` require maps: nil, lists/vectors, and scalars in map positions error. Values such as nil, false, 0, and `""` are retained and overwrite previous values. Nested maps are replaced whole; nested objects and Atoms are shared, not cloned. Use unique string/keyword keys for portability, and check fields with `get` rather than relying on map equality or iteration order. Extra pairs/maps are not overloads; use nested calls:

```clojure
(let [item {:reason "selected" :status "pending"}
      result {:status "ready"}]
  (assoc (merge item result) :selection_reason (:reason item)))
```

Deploy generated code with the matching runtime exporting `assoc` and `merge`. Key equality, duplicate-key handling, and host-object support are target-specific.

**String checks:** `"nil"`, `"false"`, `"42"`, and `:name` are strings; nil, booleans, numbers, collections, functions, atoms, and regex values are not. Use `(string? value)` before string operations on external data. Quoted atoms have target-specific types; `'"text"` is a string everywhere. Use exactly one argument; incorrect arities on compiled targets are outside the portable contract.

`filter` and `keep` are ordinary two-argument functions over one list/vector. Both run eagerly, calling a named function, lambda, or local function value once per item in order, with only that item as its argument. They preserve result order and do not mutate the input collection or copy its element objects. Empty input gives an empty list without callback calls; rejecting every item also gives an empty list, not nil. Callback errors stop traversal and propagate; prior effects remain. Maps, nil collections, arbitrary iterables, transducers, async waiting, and collection mutation during traversal are outside the contract. Host arrays and argument diagnostics are covered by the target references.

```clojure
(filter (fn [message] (= (:type message) "text")) messages)
(keep (fn [event] (if (:seen event) nil {:card (:id event)})) events)
```

`run!` is an ordinary function, not a macro. Pass a named function, lambda, or local function value. Its collection contract excludes maps, `nil`, and arbitrary iterables; mutation during traversal and awaiting async callbacks are outside the contract.

**Sorting:** `sort-by` calls a named, lambda, or local unary `keyfn` once per element, eagerly from left to right, before sorting. Equal keys keep their original order. Empty input returns an empty list/vector without calling `keyfn`; singleton input calls it once. Callback errors stop traversal and propagate, retaining earlier effects. Unlike Clojure `sort-by`, keys are computed only once and there is no comparator overload.

Use either all string keys or all numeric keys in one call. Numeric keys can mix integers and finite binary64 values: order is numeric, without epsilon, and `1`/`1.0` and both signs of zero are ties. Cross-target portability remains limited by each target's numeric range. String keys (including keywords) use case-sensitive lexicographic order without locale: ASCII sorts identically everywhere; other Unicode ordering is target-specific. Nil, booleans, composite keys, mixed strings/numbers, and non-finite keys are outside the contract.

Input is a list/vector. Maps, nil collections, arbitrary iterables, and callback mutation of the input or its elements are outside the contract. Elements themselves can be any values if their computed keys are supported. Deploy with the matching runtime exporting `sort_by`.

```clojure
(sort-by (fn [event] (:priority event))
         [{:id "A" :priority 20} {:id "B" :priority 3} {:id "C" :priority 20}])
;; Returns the original events in order B, A, C.
```

**Fractional arithmetic:** `+`, `-`, `*` preserve fractional operands without truncation. A result that is exactly an integer within signed 32-bit range is normalized to an integer, including negative zero becoming `0`. Thus `(str (+ 0.5 0.5))` is `"1"`, `(= (+ 0.5 0.5) 1)` is `true`, and the corresponding `not=` is `false`. This does not establish general mixed-literal equality such as `(= 1 1.0)`. Non-integer results retain binary64 precision when reused in arithmetic; no epsilon or decimal rounding is applied. For example, `(str (+ 1 0.2) " " (* 2 0.2))` is `"1.2 0.4"`, but `(+ 0.1 0.2)` need not equal `0.3` exactly. Exponent spelling and formatting of arbitrary fractional values can differ by target; `str` is not a portable numeric serialization format. Non-finite values and overflow remain outside the portable contract.

**Representation differences:** collection/mixed-type equality and map key handling differ across targets. Avoid duplicate/non-string keys, collection equality, and ordering assumptions in portable code; consult the selected target's reference for details.

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

### Regular expressions on all targets

Regex functions are available without imports on eval, JavaScript, and Java: `(re-pattern pattern)` or `(re-pattern pattern flags)`, `(re-find regex text)`, `(re-find-groups regex text)`, and `(re-replace text regex replacement)`.

Omitted flags or `""` select case-sensitive matching; `"i"` ignores ASCII case in searches and replacements while preserving the original text in matches and groups. Other flags, including `"g"`, `"m"`, `"ii"`, and non-string values, are errors. Unicode case matching remains engine-specific.

Regex values are opaque, truthy, and not callable on all targets. Functions, bindings, collection values, destructuring, and Atom preserve them. `str` produces `#<regex>`, including inside lists/maps. Binary `=` returns false whenever either direct operand is a regex, even the same value; binary `not=` returns true. This changes previous JS/Java identity equality. Recursive collection equality, regex map keys, unary equality, and final runner output are **not** unified; stronger guarantees are eval-only. Reusing a compiled regex after any search or replacement starts a fresh operation, unaffected by previous match positions.

Portable patterns and text use printable ASCII, TAB, and LF. The shared subset includes literals, escaped metacharacters, simple positive/negative classes and ranges, `\d`/`\D`/`\w`/`\W`/`\s`/`\S` inside and outside classes, `.`, concatenation, alternation, ordinary groups, greedy/lazy `*`/`+`/`?`/bounded quantifiers, and `^`/`$`. Dot excludes LF; anchors refer to the whole string, including a strict end (`foo$` does not match `"foo\n"`), also with `"i"`. Unicode and other control characters are target-specific. POSIX/nested/intersected classes, inline flags, lookaround, and backreferences are outside the portable guarantee; native JS/Java extensions need not be rejected just because eval rejects them. Regex reader syntax is not provided.

```clojure
(re-find (re-pattern "foo([0-9]+)") "xfoo42 foo7") ; "foo42", not capture groups
(get (re-find-groups (re-pattern "post-([0-9]+)" "i") "POST-5657") 1) ; "5657"
(re-replace "foo1 foo22" (re-pattern "foo[0-9]+") "X") ; "X X"
(re-replace "export function f() {}" (re-pattern "^export ") "") ; "function f() {}"
```

`re-find` searches from the left. No match is actual falsey nil; an empty match is the truthy string `""`. Reuse a compiled regex through a binding. Text is never implicitly compiled. Each API enforces its arity and runtime types on all targets, even for empty input or absent matches. Keywords remain string values; numbers, booleans, nil, collections, and callbacks are not coerced to strings. Quoted symbols are distinct from strings on eval but may be runtime strings on compiled targets, so their rejection is not portable. Invalid patterns and argument errors name their builtin and the reason; exact messages and host exception types are not portable. Eval reports errors through its runner/CLI; an unhandled error in an executed Node/JVM program produces stderr and a nonzero exit status. The JS/Java compilation step does not execute regex calls or promise to diagnose invalid patterns.

`re-find-groups` uses the same first-match search as `re-find`, returning an ordinary list/vector: full match at index 0, then groups in opening-parenthesis order. Use `get`, `count`, or positional destructuring. A match without groups returns a one-element collection; no match returns falsey nil. An unmatched optional group or alternative retains a nil slot (actual null on JS/Java), while a participating empty group is the truthy string `""`. For example, `"(a)?(b)"` on `"b"` yields `["b" nil "b"]`, and `"(a*)b"` yields `["b" ""]`. Captures inside repeated groups follow the native engine and are not portable; repeating characters inside one capture, as in `"post-([0-9]+)"`, is portable. `re-find` always keeps its string-or-nil result.

On all targets, `re-replace` replaces all non-overlapping matches, without rescanning inserted text. Replacement is literal: `$1`, `$&`, and backslash sequences are not capture substitutions; callback replacement is unavailable. No match preserves the input, and an empty replacement deletes matches. Empty matches insert the replacement and advance over one original ASCII character, preserving it; an empty match immediately after a nonempty match at the same position is skipped. End-of-input is processed at most once, and anchors remain relative to the whole original text. For example, replacing `""` with `"-"` in `"ab"` gives `"-a-b-"`; replacing `"a*"` gives `"-b-"`. This changes the previous JS/Java native replacement behavior (`"--b-"`). Outside the portable subset, empty-match advancement and Unicode matching remain engine-specific.

## CLI and project integration

`ly2k` reads source from **stdin**. It accepts `--target eval`, `--target js`, `--target java`, or `--target fex`; default is `eval`. A source filename is not a positional argument.

```sh
ly2k --target eval < program.clj
ly2k --target js < program.clj > program.js
ly2k --target java < program.clj > user.java
ly2k --target fex < program.clj > program.fex
ly2k --unpack-fex --output-dir out < program.fex > load-order.txt
```

The Java command assumes source without `ns`; use the declared helper class filename otherwise. Evaluation prints the final scalar; JS/Java compilation writes generated source to stdout, while FEX writes a bundle. Reported parse/eval errors go to stderr with a nonzero exit status; other compilation failures can also terminate with an error. Inspect both the compiler and the host runtime/build diagnostics.

Runtime files (`language_runtime.js`, `language_runtime.java`) are **automatically supplied by the application's build system**. Do not add manual copying, compile the compiler from source, or assume its checkout is the application's working directory. Generating JS/Java source does not run it. Follow existing build/run commands for module layout, dependencies, host entry points, and tests.

FEX emits one text bundle per source, with canonical-key/Base64-RHS line pairs. See [FEX integration](references/fex.md#cli-and-integration) for the compiler-only workflow and the separate unpacker/interpreter changes.

### Printing and diagnostics

`println` and `eprintln` are available without imports on eval, JS, and Java. They accept zero or more values, format each as `str` does, join them with one space, and add a newline. With no arguments they print a blank line. `println` writes to stdout and `eprintln` to stderr. Their return values and eval's separate final-result output are described in the selected target reference.

For OpenCode installation, symlink the directory containing this `SKILL.md` into the application as `.opencode/skills/y2k-language`; its `references/` directory is included. No `opencode.json` edit is needed. Restart OpenCode after connecting or updating the skill so a new session loads it.
