## MODIFIED Requirements

### Requirement: The JavaScript compiler SHALL emit ES module code using the language runtime

The JavaScript target SHALL считать объявленный `ns` каноническим путём модуля относительно output root: точки разделяют каталоги, а последний сегмент задаёт имя `.js` файла. Target SHALL вычислять относительный префикс к output root по глубине текущего namespace и применять его к runtime import и символьным namespace requires. Для namespace из одного сегмента и исходника без `ns` префикс SHALL оставаться `./`. Символьный namespace require SHALL компилироваться в путь от output root с расширением `.js`: compiler SHALL применять стандартное symbol munging к required namespace, затем преобразовывать точки в `/`. Строковый `:require` SHALL сохранять свой module specifier как bare ESM import без munging, добавления префикса output root или `.js`. Локальный алиас string require SHALL использовать стандартное munging символов, чтобы быть допустимым JavaScript identifier. Строковые литералы SHALL сохранять исходное содержимое, включая `/`. Форма `(export-default key value ...)` SHALL принимать одну или несколько пар с keyword или string literal keys и компилироваться в статическую ESM-декларацию, значением которой является обычный JavaScript object с соответствующими own properties. Она SHALL NOT вызывать `hash_map` или `export_default`. Пустая форма, нечётное число аргументов или key, который после desugaring не представлен строковым atom, SHALL приводить к ошибке JavaScript compilation. Public top-level `def` SHALL compile to an `export const` binding; top-level `def` with private metadata SHALL compile to a non-exported `const` binding.

#### Scenario: Runtime import
- **WHEN** source без `ns` или с `(ns main)` компилируется в JavaScript
- **THEN** generated module импортирует runtime functions из `./language_runtime.js`

#### Scenario: Runtime import from a nested namespace
- **WHEN** source с `(ns app.commands.add)` компилируется как `app/commands/add.js` относительно output root
- **THEN** generated module импортирует runtime functions из `../../language_runtime.js`

#### Scenario: Namespace requires
- **WHEN** source содержит `(ns main (:require [io.math.core :as mc]))`
- **THEN** JavaScript emits `import * as mc from "./io/math/core.js"`

#### Scenario: Nested namespace requires a root namespace
- **WHEN** source содержит `(ns commands.add (:require [db :as db]))` и компилируется как `commands/add.js` относительно output root
- **THEN** JavaScript emits `import * as db from "../db.js"`

#### Scenario: Nested namespace relationships
- **WHEN** source в namespace `app.commands.add` требует родительский `app.commands`, соседний `app.commands.remove`, дочерний `app.commands.add.audit` и чужой дочерний `other.feature.worker` namespaces
- **THEN** JavaScript emits imports соответственно из `../../app/commands.js`, `../../app/commands/remove.js`, `../../app/commands/add/audit.js` и `../../other/feature/worker.js`

#### Scenario: Hyphenated root namespace require
- **WHEN** source содержит `(ns main (:require [effect-fetch :as fetch]))`
- **THEN** JavaScript emits `import * as fetch from "./effect_fetch.js"`

#### Scenario: Hyphenated dotted namespace require
- **WHEN** source содержит `(ns main (:require [effects-promise.fetch :as fetch]))`
- **THEN** JavaScript emits `import * as fetch from "./effects_promise/fetch.js"`

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
- **WHEN** source contains `(export-default :fetch handler :scheduled scheduled-handler)`
- **THEN** JavaScript emits `export default {["fetch"]: handler, ["scheduled"]: scheduled_handler};`
- **AND** exported value has `Object.prototype` in its prototype chain
- **AND** output does not invoke `hash_map` or `export_default`

#### Scenario: Malformed default export
- **WHEN** source contains an empty `(export-default)`, an unmatched key, or a key that is neither a keyword nor a string literal
- **THEN** JavaScript compilation fails

#### Scenario: Ordinary hash maps retain their representation
- **WHEN** a map literal is compiled outside `export-default`
- **THEN** JavaScript continues to construct it through `hash_map`
