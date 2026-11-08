# JavaScript (`js`)

Use with the common language and library contracts in `../SKILL.md`.

## Modules and host interop

- Generates an ES module, not executed output. Public definitions become `export const`; `def-`/`defn-` become non-exported `const`. Hyphens in identifiers become underscores; punctuation is munged (for example `!` becomes `_BANG_`, `?` becomes `_QMARK_`).
- Symbolic namespace requires become relative `.js` imports. Output layout follows namespace segments: `app.commands.add` corresponds to `app/commands/add.js`. Runtime import is relative to the output root (`../../language_runtime.js` in this example). Required namespace names are munged before dots become `/`.
- String requires preserve their decoded ESM module specifier: `(:require ["node:path" :as path] ["node:url" :as url])`, for example. No extra `.js` or namespace prefix is added; aliases are munged. Java `:import` clauses do not import JavaScript modules.
- Host calls: `(new Constructor args...)` or `(Constructor. args...)`; `(. receiver method args...)` or `(.method receiver args...)`; `(alias/function args...)` for a module function. Host APIs must exist in the actual application environment. No async/await language form is provided; use host callbacks/promises where appropriate.
- `(cast TYPE value)` is transparent; Java type metadata does not provide runtime checking.
- `(export-default :key value "other-key" other-value)` is a **top-level** ESM default export of an ordinary JS object. It takes one or more key/value pairs with literal keyword/string keys, not a map argument. Empty forms, odd argument counts, or computed keys fail. Ordinary map literals still use the language's null-prototype map representation.

```clojure
(defn handle-fetch [request env ctx]
  (Response. "OK"))

(export-default :fetch handle-fetch)
```

The example requires a host with `Response`, such as a current Node or a Fetch-compatible worker. The build system places the module and its runtime; use the application's runner to invoke the exported handler.

## Values and host collections

- Quote represents atoms as text, including numbers, nil, booleans, and symbols. Consequently `(string? 'foo)`, `(string? 'nil)`, `(string? 'false)`, and `(string? '42)` are true. `string?` accepts primitive strings (including JSON.parse string results), not `new String("x")`, objects, or undefined.
- Host `undefined` is falsey. `get` normalizes retrieved `undefined` to nil; `filter` rejects undefined predicate results and `keep` excludes undefined results. Language truthiness still applies: `0`, `""`, `"nil"`, and `"false"` are truthy.
- Collections use identity equality. Map keys are stringified, duplicates overwrite, and integer-like keys may be reordered.
- `assoc` and `merge` accept ordinary JSON objects, copy own enumerable string fields, and return a null-prototype object where `__proto__` and `constructor` are ordinary data keys. Class instances, getters, proxies, and symbol keys are outside this contract.
- `conj`, `filter`, `keep`, `run!`, and `sort-by` accept ordinary dense host arrays. Sparse arrays and arbitrary iterables are outside their contracts. These operations preserve element objects. For `run!`, convert a DOM `NodeList` with `Array.from` first; direct `NodeList` traversal, collection mutation during traversal, and awaiting async callbacks are outside the contract.
- `filter`, `keep`, and `run!` do not promise eval's argument diagnostics. Incorrect `string?` arities are outside the portable contract.
- `sort-by` compares strings in UTF-16 order without locale; ASCII ordering is portable. Regex uses native `RegExp`, subject to the shared regex contract. Outside the portable regex subset, empty-match advancement uses one UTF-16 code unit.
- `println` / `eprintln` use `console.log` / `console.error` and return undefined.

## Synchronous exceptions

Use `(try body... (catch error handler...) (finally cleanup...))` with either
section optional, but at least one present; see the common syntax rules.
The handler receives the original thrown value, including `false`, `null`,
strings, and objects. `(throw error)` preserves it when rethrowing.

For host `Error` objects, `(get error "message")` reads the message and
`(console/error error)` logs the object with the host's stack formatting:

```clojure
(try (.parse JSON "invalid")
     (catch error (console/error error) (get error "message")))
```

Cleanup runs on synchronous exit, even after a handler error. Its error replaces
any prior result/error; the same try's catch does not catch its cleanup error.
A returned Promise is an ordinary result: cleanup has already run before later
resolution/rejection, and the catch does not observe that rejection. Handle it
through the Promise's own callbacks; try does not await it.

## Raw host code

`(raw-code "text")` is a **special form**, not a function or user macro. Use a single source string literal as its argument. Payload extraction rejects non-literal operands that remain after lowering; it does not guarantee rejection of expressions folded into literals. The parser decodes ordinary string escapes once; the compiler inserts that decoded text unchanged, without escaping, identifier munging, interpolation, parentheses, `return`, an extra `;`, or a wrapper. Supply any required host terminators yourself. An empty string inserts no code.

Use it directly at top level, or as a non-final statement in a function's sequential body (including `let`/`do`). Finish the body with an ordinary language expression such as `nil`. Expression-valued raw-code is unsupported: do not use it as a call argument, callee, binding/def RHS, condition, direct `if` branch result or final body form. For conditional statements inside a function, use `(if condition (do (raw-code "...") nil) nil)`. Quoted forms remain data. The compiler does not validate source positions before lowering or guarantee diagnostics for unsupported placements. Arity and literal checks happen when emitting the payload.

`raw-code` is reserved in call position, so a user function with that name cannot be called using this syntax. The payload is trusted host code, **not sandboxed** and not translated between targets. Host syntax, scope and type errors remain the host toolchain's responsibility. Host declarations are not registered in the compiler's function/local tables; use actual host identifiers, not assumed compiler temporary names.

Top-level `raw-code` inserts directly into the ES module, permitting host declarations/imports/exports. Within a function it inserts statements before the ordinary final expression:

```clojure
(raw-code "export const hostValue = 42;")
(defn test []
  (raw-code "console.log('hello');")
  (str hostValue))
```

## Source maps

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
