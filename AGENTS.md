# AGENTS.md

Guidance for AI coding agents working with clj2js (ly2k) - a Lisp compiler written in OCaml targeting JavaScript, Java, and an interpreter.

## Build Commands

```bash
make build          # Build project (runs prelude generation first)
make test           # Run tests
make restore        # Install opam dependencies
make deploy         # Run tests and deploy binary to ~/.local/bin/ly2k
make prelude        # Regenerate prelude.ml from prelude/data/*.clj
```

## Running Specific Tests

The test runner uses Alcotest. Run specific test suites by name:

```bash
./_build/default/test/test.exe test "Eval"     # Interpreter tests
./_build/default/test/test.exe test "JS"       # JavaScript backend tests
./_build/default/test/test.exe test "Java"     # Java backend tests
./_build/default/test/test.exe test "Sexp"     # S-expression tests
./_build/default/test/test.exe test "Common (Eval)"  # Common tests via interpreter
```

## CLI Usage

```bash
ly2k -target js -src app.clj                    # Compile to JavaScript
ly2k -target java -namespace myapp -src app.clj # Compile to Java
ly2k -target eval -src app.clj                  # Interpret/evaluate
```

## Project Architecture

### Directory Structure

| Directory   | Purpose                                                    |
|-------------|------------------------------------------------------------|
| `/bin`      | CLI entry point (`main.ml`)                                |
| `/core`     | Parser, AST types, utilities                               |
| `/macro`    | Language macros (fn, let, cond, case, if-let, ns, gen-class) |
| `/stage`    | Compilation stages (namespace resolution, type analysis)   |
| `/backend`  | Code generators (JS, Java, interpreter)                    |
| `/test`     | Alcotest test suites                                       |
| `/prelude`  | Runtime libraries and source files                         |

### Key Files

- `core/common.ml` - Core types (`sexp`, `cljexp`, `meta`, `obj`), effects, utilities
- `core/prelude.ml` - Auto-generated from `prelude/data/*.clj` (use `make prelude`)
- `bin/main.ml` - CLI with `-target`, `-src`, `-namespace`, `-log` options

### Two-Tier AST

- **cljexp** - Parser output: `Atom | RBList | SBList | CBList` (preserves bracket types)
- **sexp** - Normalized: `SAtom | SList` (used throughout compilation)
- Both carry `meta { line; pos; symbol }` for source location and type annotations

## Code Style Guidelines

### Formatting

- `.ocamlformat`: `profile = default`, `margin = 80`
- Run `dune fmt` to format

### Naming Conventions

| Element         | Convention   | Example                        |
|-----------------|--------------|--------------------------------|
| Modules         | CamelCase    | `FileReader`, `NameGenerator`  |
| Functions/types | snake_case   | `meta_empty`, `do_compile`     |
| File names      | Prefixed     | `stage_*.ml`, `backend_*.ml`   |

### Import Patterns

```ocaml
open Core__.Common              (* Access Common module from core library *)
module A = Alcotest             (* Module alias *)
module Js = Backend__.Backend_js
```

### Error Handling

```ocaml
failwith __LOC__                         (* Generic failure with location *)
failnode __LOC__ [node]                  (* Failure with cljexp node context *)
failsexp __LOC__ [sexp]                  (* Failure with sexp context *)
OUtils.failobj __LOC__ x                 (* Failure with obj context *)
```

### Effect System

```ocaml
FileReader.read filename        (* Performs Load effect *)
FileReader.with_scope f arg     (* Real file I/O handler *)
FileReader.with_stub_scope content f arg  (* Stubbed for testing *)
NameGenerator.get_new_var ()    (* Generate unique variable name *)
```

### Macro Pattern

Each macro in `/macro` returns `sexp option`:

```ocaml
let invoke simplify node : sexp option =
  match node with
  | SList (_, SAtom (_, "my-macro") :: args) -> Some (transform args)
  | _ -> None

(* Chain macros using or_val *)
Macro_case.invoke simplify node |> or_val node (Macro_cond.invoke simplify)
```

### Test Pattern

Tests use tuples of (location, input, expected):

```ocaml
let tests = [
  (__LOC__, {|(defn test [] 42)|}, "42");
  (__LOC__, {|(defn test [] (+ 1 2))|}, "3");
]
```

### Pattern Matching Style

```ocaml
let rec transform = function
  | SAtom (m, x) -> handle_atom m x
  | SList (m, SAtom (_, "fn") :: args) -> handle_fn m args
  | SList (m, xs) -> SList (m, List.map transform xs)
  | node -> failsexp __LOC__ [node]
```

### Common Utilities

```ocaml
last xs                         (* Get last element *)
butlast xs                      (* All but last element *)
List.split_into_pairs xs        (* [a;b;c;d] -> [(a,b);(c,d)] *)
debug_show_sexp [node]          (* Pretty print sexp for debugging *)
```

## Debugging Tips

1. Enable logging: `ly2k -log -target js -src app.clj`
2. Use `debug_show_sexp` to print AST nodes
3. Use `make test_slow` for fail-fast with detailed output

## Git Policy

- Do NOT auto-commit changes
- Wait for explicit user request to commit
