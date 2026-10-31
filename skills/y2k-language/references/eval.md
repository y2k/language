# Eval (`eval`)

Use with the common language and library contracts in `../SKILL.md`.

## Execution and namespaces

- Executes forms in order; globals are isolated by namespace (default `user`). Functions retain their definition namespace and lexical locals. `alias/member` resolves a declared alias; `namespace/member` can resolve directly.
- `ns` with `:require` establishes aliases, **not automatic file loading**. Java `:import` clauses have no host-interop effect here.
- `(deps {:package "version"})` loads package `.clj` files from `$LY2K_PACKAGES_DIR/PACKAGE/VERSION` at that point in execution. Both package and version must be strings; keywords remain strings, but numbers and quoted symbols are rejected. Put it before dependent references. Nested dependencies load too; each loaded file restores the caller's namespace afterward. Do not depend on directory enumeration order or assume this downloads packages.
- Eval arithmetic accepts numeric values, never numeric strings or symbols. `+`, `-`, `*` retain numbers between operations; `count` returns an integer. `/`, ordering, `drop`, and list indices require integer values, rejecting even `1.0`. Thus `(get [10 20] (+ 0.5 0.5))` returns `20`, but `(get [10 20] 1.0)` errors. Fractional division and ordering are not supported in eval.
- `(slurp "notes.txt")` requires one string path, relative to the process working directory when not absolute, and returns the full file contents as a string. Numbers and symbols such as `'file` are rejected. Use string literals for text that older eval code expressed as quoted symbols.
- `cast` ignores its type and returns the value; Java/JS host constructors and methods are not interpreted.
- Eval-only library functions such as `slurp` and `assert` are outside the cross-platform library. Do not assume they exist in compiled applications.
- Eval rejects a reached raw-code form as supported only on JS/Java, without evaluating its arguments (even malformed ones). Quoted raw-code remains data, and an unselected `if` branch is not reached: `(if false (raw-code "...") "ok")` returns `"ok"` on eval.

## Values, equality, and collections

Eval preserves quoted literal types: `'42` is an integer, `'0.25` a binary64 number, `'false` a boolean, `'nil` nil, and `'"42"` a string. Other quoted atoms such as `'foo` are symbols, distinct from `"foo"`. Quoted lists recursively contain quoted values; the common caveat about macro expansion inside quoted content still applies.

In eval, `(= nil "nil")` is `false`, `(if "nil" 1 2)` is `1`, and `(str nil)` is the truthy string `"nil"`. Nil and text `"nil"` also remain distinct in collections and as map keys; `get`/`get-in` reject traversal through text `"nil"` as through other strings. Use the literal `nil` for missing data; quoted nil is not portable across targets.

Eval also distinguishes booleans from text: `(= false "false")` is `false`, `(= 'false false)` is `true`, and `(= 'false "false")` is `false`. Predicates and comparisons return actual booleans. Boolean and text keys remain distinct in eval maps and destructuring. `(str false)` returns the truthy string `"false"`, so `(if (str false) 1 2)` returns `1`. This changes older eval versions where text `"false"` was falsey; use `(= value "false")` when testing for that text.

Eval distinguishes nil, booleans, strings, symbols, machine integers, and binary64 numbers. `(= 42 "42")` and `(= 'foo "foo")` are false. Eval compares integers and binary64 numbers by exact numeric value: `(= 1 1.0)` is true, without epsilon or rounding a large integer to binary64. This mixed equality is not a cross-target guarantee. Eval compares lists/maps recursively (map pair order matters); functions and Atom are unequal even to themselves.

Eval map lookup, `get-in`, and associative destructuring use the same equality as `=` and return the first matching pair. For `(hash-map 1 "first" 1.0 "second" "1" "text")`, both numeric keys retrieve `"first"`, the string key retrieves `"text"`, and `count` remains 3. Symbol and string keys are distinct; function/Atom keys never match, even using the same reference.

On eval, updates use `=` for keys. Assoc replaces all equal-key pairs with one pair; merge applies right-hand pairs left to right, so the last duplicate on the right wins. Existing `hash-map` construction and first-match `get` stay unchanged.

Eval rejects a list destructuring pattern applied to a non-list or a map pattern applied to a non-map, including missing nested collections. Missing leaves still become nil. `conj` rejects incorrect arities and collection types.

Quoted `'foo`, `'nil`, `'false`, and `'42` are not strings on eval, so `string?` returns false for them; `'"text"` is a string.

`sort-by` preserves the order of large integers against binary64 without rounding the integer. String keys use byte order, without locale; ASCII ordering is portable.

## Argument contracts

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

## Regular expressions

The portable API and patterns are in the common regex section of `../SKILL.md`. The eval dialect is **OCaml Re.Perl**, using default options or `Caseless` for `"i"`: a Perl-style subset, not Java Pattern or full Perl/PCRE compatibility. It supports literals, character classes (including `\\d`, `\\s`, `\\S` in language string literals), alternation, groups, greedy/lazy quantifiers, and anchors. Matching is byte-based; Unicode text is not normalized and `.` can select one byte of a UTF-8 character. `.` excludes newline; `^`/`$` match the start/end of the whole string (`foo$` does not match `"foo\n"`). Eval rejects inline flags such as `(?m)`/`(?s)`, lookaround, and backreferences. For multiline extraction use `[\\s\\S]*?` in a language string. Regex literals `#"..."` are not supported. Outside the portable subset, empty-match advancement uses one byte.

The project uses Re **1.13.2**, pinned to avoid a Re 1.14.0 regression in bracketed `\W`. Both `"\\W+"` and `"[\\W]+"` match non-word characters; neither matches `"abc_42"`.

Eval regex values are opaque and truthy. Bindings, functions, collections, destructuring, and Atom preserve them. Like functions/Atom, regex values are unequal even to themselves in binary equality, including nested collection comparisons; regex map keys never match. Unary `=`/`not=` still return true/false. `(str (re-pattern "x"))` returns `"#<regex>"`; a final regex produces an empty runner result, so use `str` to inspect it. Regex values are not callable.

## CLI output and diagnostics

CLI output is only the final scalar: strings and symbols print without quotes, numbers use `str` formatting, nil prints `nil`, and booleans print `true`/`false`. Original numeric spelling is not preserved: `1.00` prints `1`. A final list, map, function, or Atom produces an empty result string; use `str` to inspect it. `str` always returns a string and also renders typed values inside collections, for example `(str [false true])` returns `"(false true)"`. Defining `test` does not call it: append `(test)` when evaluating such examples.

`println` and `eprintln` return nil. Eval CLI prints the final result separately: `(eprintln "hello")` writes `hello\n` to stderr and `nil\n` to stdout, with exit status 0. `(println "hello")` writes `hello\nnil\n` to stdout. For diagnostics alongside a useful result:

```clojure
(do (eprintln "processing") "result")
```

On eval this writes `processing\n` to stderr and `result\n` to stdout. Use `eprintln` to keep diagnostics out of redirected result output.

## Text-based userscript assembly

For known generated inputs with a single initial runtime import and no `export ` inside JS strings, save this as `build.clj`:

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

Run `ly2k --target js < app.clj > compiled.js`, then `ly2k --target eval < build.clj > app.user.js` from the directory containing the three inputs. `header.txt` must include the `// ==UserScript==` and `// ==/UserScript==` delimiters. Have the application's build supply `language_runtime.js` beside these inputs (the compiler distribution's runtime source is `prelude/language_runtime.js`). This is a text transformation for that input format, not a JavaScript parser or general bundler. This assembly script runs on eval; the portable regex API also works on JS/Java.
