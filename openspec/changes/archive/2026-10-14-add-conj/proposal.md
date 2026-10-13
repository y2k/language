# Proposal

## Why

В issue #32 пакет `effect/0.1.0/js/effect.clj` накапливает результаты через отсутствующий `conj`; generated JS оставляет неразрешённый вызов. Аналогичная зависимость есть в других пакетах, поэтому согласовано добавить общую операцию языка для eval, JS и Java.

## What Changes

- Предоставить без импорта двухаргументный `(conj items value)` для общей list/vector-модели: новая коллекция с одним элементом в конце, без изменения исходника.
- Сохранять значения и ссылки поверхностно, включая nil, false и вложенные коллекции как отдельные элементы.
- Подключить `conj` к JS runtime import и описать узкий переносимый контракт в справочнике.
- Проверить переносимое накопление и JS Promise-цепочку по образцу `effect/batch`.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `eval-runtime`: обычный stdlib binding conj для добавления одного элемента в конец list/vector.
- `compiler-targets`: доступность и та же семантика conj в JS/Java, совместимость JS с накоплением в Promise-цепочке.

## Impact

Затронуты `backend_eval/eval_stdlib.ml`, `prelude/language_runtime.js`, `prelude/language_runtime.java`, `backend_compiler/js.ml`, тесты и `skills/y2k-language/SKILL.md`. Новые зависимости не требуются. Compiler и runtime необходимо поставлять совместно; существующий `Makefile` копирует runtime в пакет prelude. Изменение не включает редактирование внешнего репозитория packages, общую диагностику неразрешённых символов из #27 или полную Clojure-семантику conj.
