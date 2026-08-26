## Why

JavaScript-компилятор не различает public и private top-level definitions. В частности, `(def- storage value)` компилируется как вызов несуществующей функции `def_`, а public forms не экспортируются из ESM-модуля.

## What Changes

- Добавить private metadata к AST-формам и сохранять её при macro expansion.
- Раскрывать `def-` в core `def` с private metadata.
- Раскрывать `defn-` в `defn` с private metadata, затем применять обычные macro expansions до core `def` и `fn*`.
- Генерировать `const` для private `def` и `export const` для public `def` в JavaScript.
- **BREAKING**: JavaScript output для public `def` и `defn` становится ESM export, а не локальным `const` binding.

## Capabilities

### New Capabilities

_Нет._

### Modified Capabilities

- `frontend-syntax`: private declaration macros и повторное macro expansion с сохранением metadata.
- `compiler-targets`: ESM-экспорт public definitions и private bindings в JavaScript.

## Impact

- Затронуты `frontend/ast.ml`, `frontend/parser.ml`, `frontend/desugar.ml`, `frontend/builtin_macros.ml` и `backend_compiler/js.ml`.
- Затронуты frontend и JavaScript compiler tests; Java и eval продолжают получать обычную core-форму `def`.
