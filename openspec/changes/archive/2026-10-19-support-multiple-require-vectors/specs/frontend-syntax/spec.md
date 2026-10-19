# Spec Delta

## MODIFIED Requirements

### Requirement: The namespace macro SHALL lower namespace declarations

The `ns` macro SHALL lower namespace declarations into `compiler/ns` with string namespace, require pairs, and import pairs. An `:import` clause SHALL accept one or more bracket import vectors; each vector SHALL contain a package symbol followed by zero or more class symbols. The macro SHALL emit import pairs in source order across vectors and classes. Секция `:require` SHALL принимать один или несколько векторов `[namespace :as alias]` и выдавать пары в исходном порядке внутри секций и между ними. Пустая секция `(:require)` SHALL отклоняться. Объединение require-векторов в одну секцию SHALL сохранять поведение отдельных секций на eval, JS и Java.

#### Scenario: Lower require aliases
- **WHEN** the input contains `(ns app.main (:require [io.math.core :as mc]))`
- **THEN** it becomes a `compiler/ns` form with the namespace `io.math.core` paired to alias `mc`

#### Scenario: Lower import classes
- **WHEN** the input contains `(ns app.main (:import [java.time LocalDate]))`
- **THEN** it records `LocalDate` mapped to `java.time.LocalDate`

#### Scenario: Lower multiple import vectors
- **WHEN** the input contains `(ns app.main (:import [java.time LocalDate] [java.util UUID]))`
- **THEN** it records `LocalDate` mapped to `java.time.LocalDate` followed by `UUID` mapped to `java.util.UUID`

#### Scenario: Несколько require-векторов в одной секции
- **WHEN** вход содержит `(ns probe (:require [alpha.core :as a] [beta.core :as b]))`
- **THEN** раскрытие даёт `(compiler/ns "probe" (("alpha.core" "a") ("beta.core" "b")) ())`

#### Scenario: Порядок между несколькими секциями
- **WHEN** вход содержит `(ns probe (:require [alpha.core :as a] [beta.core :as b]) (:import [java.time LocalDate]) (:require [gamma.core :as c]))`
- **THEN** require-пары идут в порядке `alpha.core`, `beta.core`, `gamma.core`
- **AND** import-пара для `LocalDate` сохраняется

#### Scenario: Эквивалентность отдельных секций на targets
- **WHEN** корректная программа объединяет отдельные `:require` секции в одну, сохраняя порядок их векторов
- **THEN** eval разрешает те же aliases к тем же значениям
- **AND** JS и Java генерируют тот же код, что для отдельных секций, при компиляции без source map

#### Scenario: Java-пример из issue 35
- **WHEN** для Java компилируются `(ns probe (:require [alpha.core :as a] [beta.core :as b]))` и `(defn check [] (b/value (a/value)))`
- **THEN** тело метода содержит `return beta.core.value(alpha.core.value());`

#### Scenario: Несколько строковых JS-модулей
- **WHEN** для JS компилируется `(ns probe (:require ["node:path" :as path] ["node:url" :as url]))`
- **THEN** генерируются импорты `node:path` с alias `path` и `node:url` с alias `url` в этом порядке, как при отдельных секциях

#### Scenario: Пустая секция require
- **WHEN** вход содержит `(ns probe (:require))`
- **THEN** раскрытие `ns` завершается ошибкой
