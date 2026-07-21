# Y2K Language

Y2K Language is an experimental Lisp-inspired programming language implemented in OCaml. It parses and desugars S-expressions, then either evaluates them directly or generates JavaScript ES modules and Java source.

## Highlights

- Direct evaluation with `eval`
- JavaScript and Java compilation targets
- Functions, lexical bindings, conditionals, collection literals, and quoted forms
- Sequential and associative destructuring in `let` bindings and function parameters
- `and`, `or`, `->`, and `->>` macros
- Namespaces, qualified references, and dependency loading for the evaluator
- Java imports, constructors, method calls, typed lambdas, and `gen-class`

The syntax takes inspiration from Clojure, but Y2K Language is not a Clojure implementation.

## Requirements

- OCaml and Dune
- Node.js to execute generated JavaScript
- A JDK to compile generated Java

## Build and Test

```sh
make build
make test
```

Use `make test_smoke` to stop at the first failing test.

## Quick Start

Evaluate a program from standard input:

```sh
printf '(str "Hello, " "world!")\n' | dune exec ./bin/main.exe -- --target eval
```

Expected output:

```text
Hello, world!
```

Functions and collection operations use familiar S-expression syntax:

```clojure
(defn duplicate [value]
  (str value value))

(reduce str (map duplicate [1 2 3]))
;; => "112233"
```

## Compiler Targets

The CLI reads source from standard input and writes either the evaluated value or generated source to standard output.

| Target | Command | Result |
| --- | --- | --- |
| `eval` | `--target eval` | Evaluates the program |
| `js` | `--target js` | Generates an ES module |
| `java` | `--target java` | Generates Java source |

Generate and run JavaScript:

```sh
dune exec ./bin/main.exe -- --target js < program.clj > program.js
cp prelude/language_runtime.js .
node --input-type=module < program.js
```

Generated JavaScript imports `./language_runtime.js`; keep the runtime alongside the generated module.

Generate Java source:

```sh
dune exec ./bin/main.exe -- --target java < program.clj > user.java
javac -d out prelude/language_runtime.java user.java
```

Generated Java source statically imports `y2k.language.language_runtime`. A source file without an `ns` declaration produces the helper class `user`.

## Language Features

### Bindings and Destructuring

```clojure
(let [[first second] ["Ada" "Lovelace"]]
  (str second ", " first))

(let [{:name name :age age} {"name" "Ada" "age" 36}]
  (str name " is " age))
```

### Threading Macros

```clojure
(-> "hello"
    (.toUpperCase)
    (str "!"))
```

### Namespaces and Java Interoperability

```clojure
(ns app.example
  (:import [java.time LocalDate]))

(defn today []
  (LocalDate/of 2024 1 2))
```

Java interop supports constructor shorthand such as `(LocalDate. ...)`, method shorthand such as `(.toString value)`, explicit casts, typed lambdas, and `gen-class`.

### Evaluator Dependencies

For the `eval` target, `(deps {...})` loads package `.clj` files from:

```text
$LY2K_PACKAGES_DIR/PACKAGE/VERSION
```

Load dependencies before referring to their definitions.

## Project Layout

| Path | Purpose |
| --- | --- |
| `frontend/` | Parser, AST, macro expansion, and desugaring |
| `backend_eval/` | Evaluator and standard library |
| `backend_compiler/` | JavaScript and Java compilers |
| `prelude/` | JavaScript and Java runtime support |
| `bin/` | Shared runner and command-line entry point |
| `test/` | Alcotest suites and cross-target language samples |

## Current Limitations

- `def` is supported only at top level.
- Java source must contain top-level definitions, an optional `ns` declaration, and optional `gen-class` declarations.
- Java function values support arities from zero to two.
- `gen-class` currently supports only `void` methods.

## License

Y2K Language is licensed under the [GNU General Public License v3.0](LICENSE).
