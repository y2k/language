# Java (`java`)

Use with the common language and library contracts in `../SKILL.md`.

## Classes, functions, and host interop

- Generates a public helper class with static definitions and the language runtime import. No `ns` gives class `user`; `(ns app.main)` gives package `app`, class `main`, so the generated source filename must be `main.java`. Use the build system's Java entry point; no `main` method is generated automatically.
- Source must contain top-level definitions, optional initial `ns`, and optional `gen-class`. Put executable expressions inside functions, not at Java file top level.
- Top-level functions and ordinary language function values support arities **0–4**. Function results can be called directly, including factory results, values from `get`, and selected lambdas. A Java host interface with a method named `call` is not automatically a language function: use `(.call receiver ...)` for that host method.
- Functions are generated as static methods with `Object` parameters/results and may throw exceptions. Ordinary top-level definitions become `public static` methods or fields; `def-`/`defn-` become `private static`. Private members remain accessible within their namespace, including nested `gen-class` classes, but access from another namespace is rejected by `javac`, even in the same Java package. Migrate external uses of private-marked definitions by making them public or exposing a public wrapper. Pass explicit lambdas for builtin callbacks: Java runtime methods are not general function values. Top-level function references currently use helper name `user`; in named namespaces prefer an explicit lambda calling the function rather than a bare method value.
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

Results carried out of complex `let`/`do`/`if`/`try` expressions may have Java type `Object`, even when a binding inside them has a more specific inferred type. For host interop, annotate the receiving binding or cast the whole result:

```clojure
(let [^String text (let [x (new String "a")] x)]
  (.concat text "b"))
;; Returns "ab". Alternatively: (.concat (cast String (let [...] ...)) "b").
```

Code relying on an inner binding's inferred type escaping its scope may need this explicit hint or cast. Metadata such as `^String (let ...)` is not a general expression cast; type hints are supported on bindings and parameters, with separate annotations for lambdas.

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

## gen-class

`(gen-class :name NAME :extends BASE :methods [[method [ARG-TYPES...] void] ...])` emits a public static nested subclass inside the helper. Define each implementation as top-level `(defn -method [this args...] body...)`. New and overriding public `void` instance methods are supported; static and non-void methods are not. Generated methods have no `@Override` annotation: overriding follows Java's signature and inheritance rules, with no separate check of overriding intent. Without metadata, the method calls only the helper implementation. `^override` on the declared method calls `super.method(...)` first, then the helper after normal completion; it requires an accessible concrete superclass method and is not needed to override without calling `super`. The total implementation arity, including `this`, must fit Java's 0–4 limit. A qualified `:name` must match the namespace/package rules; prefer one declaration with a simple nested class name per source file. It generates a nested class, not a separate top-level source file.

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

## Runtime values

- Quote represents atoms as text, including numbers, nil, booleans, and symbols. Consequently `(string? 'foo)`, `(string? 'nil)`, `(string? 'false)`, and `(string? '42)` are true. `string?` accepts `java.lang.String`, not `StringBuilder`. Incorrect arities on compiled targets are outside the portable contract.
- Collection equality uses host `equals`. Map keys are stringified and duplicates overwrite.
- Runtime methods for `+`, `-`, `*` return `Number` (`Integer` or `Double`) rather than always `Integer`. Recompile Java consumers with the updated runtime and deploy it together with the compiler's matching runtime version.
- `filter`, `keep`, and `run!` do not promise eval's argument diagnostics.
- `sort-by` compares strings in UTF-16 order without locale; ASCII ordering is portable. Regex uses `java.util.regex.Pattern`, subject to the shared regex contract. Outside the portable regex subset, empty-match advancement uses one UTF-16 code unit.
- `println` and `eprintln` return null.

## Synchronous exceptions

`(try body... (catch error handler...) (finally cleanup...))` uses native Java
statements. At least one section is required; see the common syntax rules.
The handler binds the original `java.lang.Exception`, including checked
exceptions. `Error` bypasses catch but still runs cleanup. Use `.getMessage`
or host logging on the caught object, and `(throw error)` to rethrow it:

```clojure
(try (Class/forName "no.such.Class")
     (catch error (.getMessage error)))
```

For host stack logging, `(.printStackTrace error)` is a void call: place it
before the handler's final value. Discarded try and final try in a
`^void:java.lang.Runnable` callback may end in a host void call. A value-returning
body still needs a value. Checked exceptions pass through the existing
callback/gen-class/static-initialization bridges without replacing their identity;
uncaught class-initialization failures retain Java's native wrapping rules.

Cleanup's value is ignored. A cleanup exception replaces any earlier result or
exception, with no automatic cause or suppressed exception; its own catch cannot
intercept it. `(throw expression)` evaluates its operand once and accepts a
`Throwable`; null/non-Throwable values follow Java's NPE/cast behavior.
Generated throw calls runtime `throw_value`, backed by `sneaky_throw`. Update the
compiler and Java runtime together. Try needs no Supplier or helper callback.

## Resource cleanup: with-open

Java supports `(with-open [name resource ...] body...)`. Bindings are simple
unqualified symbols, optionally type-hinted; destructuring is unsupported.
The macro is bundled as an internal language definition and needs no import.
Initializers run once, left to right, and can use earlier bindings. Resources
close once in reverse order, including on exceptions. If an initializer fails,
previous resources close and the body does not run.

```clojure
(with-open [^java.io.Reader r (new java.io.StringReader "A")]
  (.read r))
;; Returns 65 after closing the reader.
```

Cleanup calls `.close` after casting to `java.lang.AutoCloseable`, so a factory
returning `Object` needs no hint for cleanup. Resource-specific methods in the
body still need ordinary hints/casts. An incompatible `Object` fails at cleanup
with `ClassCastException`; `nil` fails with `NullPointerException`, after the
body runs. Statically incompatible types such as `String` may fail in `javac`.

The result is the last body value after successful cleanup; an empty body
returns `nil`. An empty binding vector simply executes the body. Both value
and discarded-result positions are supported. A value-returning body must end
in a value rather than a host void call.

Semantics follow nested `try/finally`: a close exception replaces the body
result or pending exception, without automatic suppressed exceptions. Outer
resources still close; if they also fail, the outermost failing close wins.
This form's supported execution target is Java, not eval, JS, or FEX.

## Raw host code

`(raw-code "text")` is a **special form**, not a function or user macro. Use a single source string literal as its argument. Payload extraction rejects non-literal operands that remain after lowering; it does not guarantee rejection of expressions folded into literals. The parser decodes ordinary string escapes once; the compiler inserts that decoded text unchanged, without escaping, identifier munging, interpolation, parentheses, `return`, an extra `;`, or a wrapper. Supply any required host terminators yourself. An empty string inserts no code.

Use it directly at top level, or as a non-final statement in a function's sequential body (including `let`/`do`). Finish the body with an ordinary language expression such as `nil`. Expression-valued raw-code is unsupported: do not use it as a call argument, callee, binding/def RHS, condition, direct `if` branch result or final body form. For conditional statements inside a function, use `(if condition (do (raw-code "...") nil) nil)`. Quoted forms remain data. The compiler does not validate source positions before lowering or guarantee diagnostics for unsupported placements. Arity and literal checks happen when emitting the payload.

`raw-code` is reserved in call position, so a user function with that name cannot be called using this syntax. The payload is trusted host code, **not sandboxed** and not translated between targets. Host syntax, scope and type errors remain the host toolchain's responsibility. Host declarations are not registered in the compiler's function/local tables; use actual host identifiers, not assumed compiler temporary names.

Top-level `raw-code` inserts **inside the generated helper class**, in source order. Write fields, methods, nested classes or explicit static blocks yourself; it does not automatically create a static initializer or move `import` outside the class. Within a function it inserts statements:

```clojure
(raw-code "public static int hostValue = 42;")
(defn test []
  (raw-code "System.out.println(\"hello\");")
  (str hostValue))
```
