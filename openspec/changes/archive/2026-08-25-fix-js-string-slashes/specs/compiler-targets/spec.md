## MODIFIED Requirements

### Requirement: The JavaScript compiler SHALL emit ES module code using the language runtime

The JavaScript target SHALL emit an import from `./language_runtime.js` and compile language forms to JavaScript expressions/statements. Символьный namespace require SHALL компилироваться в относительный module specifier с расширением `.js`; строковый `:require` SHALL сохранять свой module specifier как bare ESM import без добавления `./` или `.js`. Строковые литералы SHALL сохранять исходное содержимое, включая `/`. Форма `(export-default expression)` SHALL компилироваться в статическую ESM-декларацию `export default <expression>` вместо вызова `export_default(...)`.

#### Scenario: Runtime import
- **WHEN** JavaScript source is generated
- **THEN** it begins with imports for runtime functions such as `list`, `hash_map`, `truthy`, `str`, arithmetic, `count`, `map`, and `reduce`

#### Scenario: Namespace requires
- **WHEN** source contains `(:require [io.math.core :as mc])`
- **THEN** JavaScript emits `import * as mc from "./io/math/core.js"`

#### Scenario: String Node module require
- **WHEN** source contains `(:require ["node:test" :as t])`
- **THEN** JavaScript emits `import * as t from "node:test"`

#### Scenario: String npm package require
- **WHEN** source contains `(:require ["wrangler" :as w])`
- **THEN** JavaScript emits `import * as w from "wrangler"`

#### Scenario: String literal with slash
- **WHEN** source contains `(str "/")`
- **THEN** JavaScript emits `(str)("/")`

#### Scenario: Definitions and functions
- **WHEN** a top-level `def` contains a function
- **THEN** JavaScript emits a `const` binding to an arrow function

#### Scenario: JavaScript interop syntax
- **WHEN** source uses `new` or `.` forms
- **THEN** JavaScript emits constructor calls and instance method calls

#### Scenario: Default export
- **WHEN** source contains `(export-default {:fetch handler})`
- **THEN** JavaScript emits `export default (hash_map)("fetch", handler);`
- **AND** output does not invoke `export_default`
