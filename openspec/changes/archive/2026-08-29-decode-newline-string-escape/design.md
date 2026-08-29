## Context

See `proposal.md` for motivation. `frontend/parser.ml` currently represents strings as quoted `SAtom` values and preserves every backslash pair textually. The evaluator removes the surrounding quotes without decoding, while JavaScript and Java pass the payload to `Target_syntax.string_literal`, which correctly escapes a real LF for each target language.

Because all targets consume the same parsed representation, source escape semantics must be established before target dispatch. The AST does not have a separate string node, so the change must preserve the existing quoted `SAtom` representation.

## Goals / Non-Goals

**Goals:**
- Decode source `\n` once at the parser boundary.
- Preserve one shared runtime string value across `eval`, JavaScript and Java.
- Leave source locations based on original input offsets.
- Make the distinction between LF and literal backslash-plus-`n` directly testable.

**Non-Goals:**
- Define or change `\\`, `\"`, `\r`, `\t`, Unicode escapes, unknown escapes, or trailing-backslash errors.
- Introduce a string-specific AST variant or a general unescape layer.
- Change evaluator or compiler target escaping.

## Decisions

### Decode at the parser boundary

The quoted-string parser will map the pair `\n` to an OCaml string containing one LF and retain the current textual handling for every other backslash pair. Evaluation and compilation will then consume the same decoded payload through existing paths.

Decoding in each backend was rejected because it would duplicate language semantics across `backend_eval/`, JavaScript and Java. Changing `Target_syntax.string_literal` was rejected because that function correctly escapes runtime values for target source and cannot distinguish a source escape from an intentional backslash.

### Limit decoding to `\n`

Only the behavior required by the issue will change. In particular, conventional handling of `\\` is not inferred as part of this change, so strings intended to contain literal escape text remain covered by the narrowed open question.

Implementing a complete conventional escape table was rejected because it would introduce additional compatibility decisions and acceptance criteria unrelated to the reproduced defect.

### Verify the semantic boundary and real backends

A focused frontend test will inspect the parsed `SAtom` payload and compare it with an OCaml expected value containing a real LF. This prevents a false positive in which both actual and expected values pass through the same parser behavior.

A common sample fixture will compare `"a\nb"` with a source string containing a physical LF. Existing sample infrastructure will execute that fixture on `eval`, JavaScript and Java. Comparing values inside the program was chosen over asserting multiline process output because the sample harness trims output and stores expected output on one comment line.

Separate evaluator and per-compiler output tests were rejected as redundant: the focused parser test fixes the exact representation, while the common fixture executes both generated targets.

## Risks / Trade-offs

- [Programs relying on visible `\n` change behavior] -> Mark the semantic correction as breaking and limit it to the explicitly requested sequence.
- [The language still lacks a complete escape model] -> Keep every sequence except `\n` in `open-questions` for a separate decision.
- [Decoded LF appears in structural string positions such as module specifiers] -> Apply the same source-string semantics consistently; validation of domain-specific string contents remains outside this change.
- [Pretty-printed AST or diagnostics can contain a physical newline] -> Accept this as a consequence of storing the runtime payload in the existing `SAtom`; do not add a serializer redesign.

## Migration Plan

No persisted data or runtime dependency migration is required. Existing source using `\n` for a newline starts working without edits. Source intentionally relying on visible backslash-plus-`n` must be reviewed; this change does not standardize a replacement spelling because `\\` semantics remain out of scope. Rollback consists of restoring the parser's textual preservation of `\n`.
