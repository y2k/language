## MODIFIED Requirements

### Requirement: The JavaScript compiler SHALL emit ES module code using the language runtime

The JavaScript target SHALL emit an import from `./language_runtime.js` and compile language forms to JavaScript expressions/statements. Символьный namespace require SHALL компилироваться в относительный module specifier с расширением `.js`; строковый `:require` SHALL сохранять свой module specifier как bare ESM import без добавления `./` или `.js`. Локальный алиас string require SHALL использовать стандартное munging символов, чтобы быть допустимым JavaScript identifier. Строковые литералы SHALL сохранять исходное содержимое, включая `/`. Форма `(export-default expression)` SHALL компилироваться в статическую ESM-декларацию `export default <expression>` вместо вызова `export_default(...)`. Public top-level `def` SHALL compile to an `export const` binding; top-level `def` with private metadata SHALL compile to a non-exported `const` binding.

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

#### Scenario: String require with a hyphenated alias
- **WHEN** source contains `(:require ["node:async_hooks" :as async-hooks])`
- **THEN** JavaScript emits `import * as async_hooks from "node:async_hooks"`

#### Scenario: String literal with slash
- **WHEN** source contains `(str "/")`
- **THEN** JavaScript emits `(str)("/")`

#### Scenario: Definitions and functions
- **WHEN** source contains a top-level `(defn handler [request] request)`
- **THEN** JavaScript emits an `export const` binding to an arrow function

#### Scenario: Public value definition
- **WHEN** source contains a top-level `(def storage value)`
- **THEN** JavaScript emits `export const storage = value`

#### Scenario: Private function definition
- **WHEN** source contains a top-level `(defn- helper [value] value)`
- **THEN** JavaScript emits a non-exported `const` binding to an arrow function

#### Scenario: Private value definition
- **WHEN** source contains a top-level `(def- storage value)`
- **THEN** JavaScript emits `const storage = value` and does not emit `def_`

#### Scenario: JavaScript interop syntax
- **WHEN** source uses `new` or `.` forms
- **THEN** JavaScript emits constructor calls and instance method calls

#### Scenario: Default export
- **WHEN** source contains `(export-default {:fetch handler})`
- **THEN** JavaScript emits `export default (hash_map)("fetch", handler);`
- **AND** output does not invoke `export_default`
