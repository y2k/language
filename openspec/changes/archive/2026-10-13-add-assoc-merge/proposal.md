# Proposal

## Why

В issue #34 приложение events обновляет записи через `Object.assign`: в библиотеке нет переносимых операций добавления поля и объединения map. Нужны две небольшие функции с фиксированной арностью и сохранением исходных данных.

## What Changes

- Добавить `(assoc map key value)` с ровно тремя аргументами и `(merge left right)` с ровно двумя на eval, JS и Java.
- Возвращать новую map; при конфликте `merge` побеждает правая map, включая значения `false` и `nil`.
- Выполнять только поверхностное обновление. Аргументы-map обязательны: `nil` не трактуется как пустая map.
- Документировать переносимость строковых/keyword ключей и существующие различия представления map.
- Не добавлять variadic overloads, обновление векторов, `assoc-in` или deep merge.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `eval-runtime`: обычные bindings `assoc` и `merge`, использующие существующее равенство ключей и неизменяемое обновление map.
- `compiler-targets`: те же операции в JS/Java runtimes и доступность из compiled source, включая обычные JSON-объекты JS.

## Impact

Изменения затронут `backend_eval/eval_stdlib.ml`, `prelude/language_runtime.js`, `prelude/language_runtime.java`, runtime import в `backend_compiler/js.ml`, общие и целевые тесты, английский справочник `skills/y2k-language/SKILL.md`. Новые зависимости, синтаксис и этапы компиляции не нужны. Обновлённый JS compiler необходимо использовать с runtime, экспортирующим новые функции.
