## 1. Tests

- [x] 1.1 Add eval tests for `let` sequential destructuring, associative destructuring, missing map keys, and nested patterns.
- [x] 1.2 Add JavaScript target tests or samples for sequential, associative, and nested `let` destructuring.
- [x] 1.3 Add Java target tests or samples for sequential, associative, and nested `let` destructuring.

## 2. Eval Implementation

- [x] 2.1 Generalize existing eval parameter destructuring into reusable binding-pattern logic.
- [x] 2.2 Update `let*` binding evaluation to evaluate each RHS once and bind names through the reusable pattern logic.
- [x] 2.3 Support associative patterns represented as `(hash-map key binding ...)`, binding absent keys to `nil`.
- [x] 2.4 Keep existing `fn*` destructuring behavior passing through the shared binding logic.

## 3. Compiler Implementation

- [x] 3.1 Update expression lowering so destructuring `let*` bindings preserve single RHS evaluation with `Gensym` temporaries.
- [x] 3.2 Update JavaScript codegen to emit leaf variable bindings for sequential and associative patterns.
- [x] 3.3 Update Java codegen to emit leaf variable bindings for sequential and associative patterns and track generated locals.
- [x] 3.4 Ensure compiler-generated identifiers use `Gensym` and no hardcoded `__tmp` names.

## 4. Verification

- [x] 4.1 Run `ocamlformat -i` on changed `.ml`/`.mli` files.
- [x] 4.2 Run `make test_smoke`.
- [x] 4.3 Run `make test`.
