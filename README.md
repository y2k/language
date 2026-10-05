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

The JavaScript and Java runtime sources are tracked as regular files in `prelude/`.
After a successful `dune build`, `make build` copies them to
`$LY2K_PACKAGES_DIR/prelude/1.0.0/{js,java}/`, overwriting the package copies.
Set the existing `LY2K_PACKAGES_DIR` environment variable before building.
Edit the sources in `prelude/`, not the package copies.
Both test commands also run this build step.

## Docker

The image targets `linux/amd64`. Docker on ARM hosts needs amd64 emulation.
Pull the published image, or build it locally from the repository root:

```sh
docker pull --platform linux/amd64 y2khub/language:latest
# Alternatively, build locally:
docker build --platform linux/amd64 -t y2khub/language:latest .
```

The multi-stage Dockerfile builds and runs `make test` for eval, JS and Java
with OCaml 5.3, Node.js 24 and JDK 25 in its builder stage. The final
`debian:bookworm-slim` image receives only the executable. No local toolchain
installation is needed; a failing test stops the Docker build.

The container reads stdin and defaults to `eval`:

```sh
printf '(str "Hello, " "world!")\n' | docker run --rm --platform linux/amd64 -i y2khub/language:latest
# Prints: Hello, world!
```

Select a target by passing the usual CLI arguments:

```sh
printf '(defn hello [] "Hello, world!")\n' | docker run --rm --platform linux/amd64 -i y2khub/language:latest --target js > program.js
printf '(defn hello [] "Hello, world!")\n' | docker run --rm --platform linux/amd64 -i y2khub/language:latest --target java > user.java
```

The image contains the CLI binary and system libraries. These targets generate
source code; executing it requires Node.js or a JDK and the matching runtime
file from this repository's `prelude/` directory.

You can use `FROM y2khub/language:latest` as a base, or copy the binary into a
compatible Debian 12 image of the same architecture:

```dockerfile
FROM debian:bookworm-slim
COPY --from=y2khub/language:latest /usr/local/bin/language /usr/local/bin/language
ENTRYPOINT ["/usr/local/bin/language"]
```

The binary uses system libraries; copying it alone does not guarantee it will
run on Alpine, `scratch`, or another architecture. Fully static linking is
planned as a separate task.

### Publishing to Docker Hub

On every push to `main`, `.github/workflows/docker.yml` checks out the code,
logs in to Docker Hub, builds the image and pushes it as
`docker.io/y2khub/language:latest`. Tests run inside the Dockerfile, not in
separate workflow steps. Test packages stay in a temporary directory in the
builder stage. A failed test or build prevents publication. Other branches
do not publish.

Before the first run:

1. Create the Docker Hub repository `y2khub/language`.
2. Create a Docker Hub access token for `y2khub` with write access to that repository.
3. In the GitHub repository's **Settings → Secrets and variables → Actions**,
   add a repository secret named `DOCKERHUB_TOKEN` with the token value.

After a successful workflow run, pull the image and run the eval example above.
The push step logs the published digest; save it to use that exact image later
as `y2khub/language@sha256:…`. The mutable `latest` tag points to the last
successful publication, which can finish out of commit order for concurrent runs.

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

Keep `language_runtime.js` in the JavaScript output root. A generated module without `ns`, or with a one-segment
namespace, also lives in that root and imports `./language_runtime.js`. A nested namespace determines its path below
the same root: `app.commands.add` maps to `app/commands/add.js`, which imports `../../language_runtime.js`. Symbolic
namespace imports use the same output-root prefix followed by the required namespace path.

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
