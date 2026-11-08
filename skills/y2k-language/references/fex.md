# FEX target

FEX compiles one source into a text bundle containing one RHS per top-level definition. Compilation does not execute initializers or load dependencies. The bundle is transport; the future interpreter consumes unpacked RHS files.

## Source subset

Use an optional initial `ns`, followed by `def` forms (including `defn`, `def-`, and `defn-`). The default namespace is `user`. Private definitions have ordinary canonical keys.

Expressions support `nil`, booleans, int32 integers, finite binary64 floats, strings, `quote`, `if`, `do`, `let`, `fn`, calls, vectors/maps, and nested destructuring in bindings and parameters. Existing frontend macros such as `cond`, `case`, `and`, `or`, `if-let`, `->`, and `->>` work when their expansion belongs to this subset. `when` is not a frontend macro.

Decimal integer literals outside int32 are rejected, including values too large for the compiler's host integer type. Use explicit fractional or exponent notation for binary64 literals.

Bindings are sequential and lexical; local names are preserved. A closure can capture outer bindings. Functions can have zero or more parameters, including six or more. Generated destructuring temporaries use `Gensym`; their numeric suffixes are not stable across repeated in-process compilations.

Name lookup prioritizes locals, then definitions in the current source (including later definitions), then builtins. Other free names refer to globals in the current namespace. Qualified names resolve a symbolic namespace alias or use the explicit namespace. A symbolic `:require` only supplies aliases; it does not load the dependency.

The builtin references are:

```text
+ - * = not= < <= > >= not str list hash-map get count assoc conj map reduce atom deref reset! swap!
```

Other targets' standard libraries are not an implicit FEX builtin API. Host forms (`raw-code`, `.`, `new`, `set!`, `cast`, `instance?`, `gen-class`, `export-default`), host imports, and string requires are rejected. This includes method/constructor shorthand and typed parameters expanding to `cast`. Top-level expressions and nested `def` are rejected.

Executable `try` and `throw` are rejected as unsupported expressions, including
inside an unselected branch. Quoted `'(try)` and `'(throw)` remain data; FEX does
not implement the JS/Java exception API.

### Quote and strings

Quote follows the existing compiled targets after frontend desugaring: atoms become strings, lists become calls to builtin `list`. Thus `'nil` is the string `nil`, while literal `nil` is a typed nil. `'[1]` quotes the already-expanded `(list 1)` and produces the strings `list` and `1`. This is not Clojure quote. Quoted host forms are data.

String contents have already been decoded by the parser; FEX does not decode them again. Supported escapes include `\n`, `\t`, `\r`, `\"`, and `\\`; unknown escapes such as `\q` and `\u0041` remain literal pairs/text.

## RHS format

An RHS is UTF-8 without a BOM, one record per LF-terminated line. Fields use a single ASCII space. The root is line 1; child references are 1-based addresses of later records in the same RHS. There is no header, empty record, `def` wrapper, or return instruction.

| Record | Meaning |
| --- | --- |
| `nil`, `true`, `false` | Typed literals |
| `int decimal` | Signed int32 |
| `float decimal` | Finite binary64, optionally exponent notation |
| `string base64` | Standard padded Base64 of UTF-8 bytes; `-` means empty string |
| `local name` | Lexical binding |
| `global canonical-key` | Definition reference |
| `builtin name` | Portable primitive reference |
| `fn body-ref params...` | Body address and named parameters; zero parameters allowed |
| `call callee-ref args...` | Callee followed by arguments in source order; zero arguments allowed |
| `if condition-ref then-ref else-ref` | Separate condition and both branches; omitted else becomes `nil` |
| `let name value-ref body-ref` | One binding; multiple bindings nest sequentially |
| `do refs...` | Expressions in order; empty `do` has no addresses |

A single-expression function body refers directly to that expression. Multiple body expressions use `do`. Child records are laid out in preorder, left to right; their presence does not imply eager branch execution.

For `(ns app.logic) (defn calculate [x] (+ x 1))`, the RHS keyed `app.logic.calculate` is exactly:

```text
fn 2 x
call 3 4 5
builtin +
local x
int 1
```

The last line also ends with LF. String examples: `"hello world"` becomes `string aGVsbG8gd29ybGQ=`, `""` becomes `string -`, and `"%0A"` becomes `string JTBB`.

## Canonical keys

Encode namespace components and the definition name separately, then join with dots. Within each component preserve `[a-z0-9_-]`, encode ASCII uppercase as `!` plus lowercase, and encode all other UTF-8 bytes as uppercase `%HH`. Namespace dots are separators; dots inside a definition name are encoded.

| Source reference | Canonical key |
| --- | --- |
| `a.b/c` | `a.b.c` |
| `a/b.c` | `a.b%2Ec` |
| `app/foo`, `app/Foo`, `app/FOO` | `app.foo`, `app.!foo`, `app.!f!o!o` |
| `App/!foo` | `!app.%21foo` |
| `app/Я`, `app/я` | `app.%D0%AF`, `app.%D1%8F` |

Bundle key, eventual filename, and `global` key are identical. Consumers use them directly, without unescaping or re-escaping. ASCII uppercase escaping preserves case distinctions on case-insensitive filesystems; filesystem filename length limits still apply.

## Bundle

Each definition contributes exactly two LF-terminated lines: its canonical key, then standard padded Base64 of the entire RHS, including all RHS line endings. Pairs preserve source order, including repeated keys. There is no header, trailer, count, or blank separator. Empty source or namespace-only source produces zero bytes.

The external Base64 layer is separate from string-record Base64. For `(ns app.logic) (def answer 42)`:

```text
app.logic.answer
aW50IDQyCg==
```

The payload decodes to `int 42` followed by LF.

## CLI and integration

```sh
printf '%s\n' '(ns app.logic) (def answer 42)' | ly2k --target fex > app.fex
ly2k --unpack-fex --output-dir out < app.fex > load-order.txt
```

This writes the exact two-line bundle above to `app.fex`. The compiler reads stdin, emits only the bundle to stdout, and creates no code files itself. `--source-map` is JS-only; `--source-file` without a map does not affect FEX output.

The unpacker creates `out` if needed, writes `out/app.logic.answer` containing exactly `int 42` with a final LF, and prints `app.logic.answer` with LF to `load-order.txt`. `--output-dir` is required. It reads one key/Base64 pair at a time, without parsing or executing FEX. Keys become filenames literally, preserving percent and case escapes; unsafe filenames and existing non-regular files (including symlinks) are rejected.

Container lines may use LF or CRLF; the final payload may omit its line terminator. Decoded bytes are preserved, including absent final LF. Empty lines are consumed, not skipped. EOF after a key and invalid Base64 fail with a nonzero exit status and a stderr diagnostic. EOF between pairs succeeds, so a bundle truncated between pairs cannot be detected.

Identical file bytes preserve the file and its mtime; changed bytes overwrite it directly. Repeated keys are processed in order, with the last value winning. stdout reports every successfully processed key, including unchanged and repeated keys. Neighboring and stale files remain; remove stale files separately when appropriate.

**Errors can leave a partially updated directory.** There is no whole-bundle preflight, staging, atomic replacement, or rollback. Use the exit status to distinguish success from a partial load-order list.

The one-source/one-output interface fits the compile rule in `packages/make/0.6.0/make.clj`, but that package still unconditionally requires `language_runtime.<target>`. It needs a separate change before it supports FEX; there is no `language_runtime.fex` to copy.

Compilation, local unpacking, delivery (for example, `adb push --sync`), and runtime loading/reload are separate stages. Loading, evaluation, and reload belong to `add-flat-definition-interpreter`. Delivery and orchestration of multiple bundles remain external. Compiling, unpacking, or delivering a bundle does not run initializers or reload definitions.
