# Tasks

## 1. Общая операция conj

- [x] 1.1 Добавить `test/samples/conj.clj` во вложенном namespace: пустой и непустой list/vector, reduce-накопление, неизменность входа, nil/false/0/пустая строка, вложенная коллекция и общие ссылки Atom. Проверить значения через count/get, бинарное равенство и str; запуск `make test` до реализации должен выявить отсутствие conj.
- [x] 1.2 Добавить conj и stdlib binding в `backend_eval/eval_stdlib.ml`, экспорт в обоих `prelude/language_runtime.*` и JS runtime import в `backend_compiler/js.ml`. Проверить общий fixture на eval, JS и Java через `make test`.
- [x] 1.3 Дополнить `test/eval_stdlib_contracts_test.ml` компактной проверкой неверной арности и nil/map вместо коллекции; проверить Eval_error с именем conj через `make test`.
- [x] 1.4 Описать conj в английском `skills/y2k-language/SKILL.md`: два аргумента, добавление в конец list/vector, поверхностное сохранение значений, границы типов и совместная поставка runtime/compiler. Сверить описание с delta specs и общим fixture.

## 2. Регрессия issue #32

- [x] 2.1 Добавить `test/samples/js/conj_batch.clj` с Promise-цепочкой из effect/batch: несколько эффектов, порядок запуска и результатов, пустой batch. Проверить новый массив и identity host-элементов в том же JS fixture. Запустить через существующий sample-runner в `make test`, ожидая успешного выполнения и проверок без ReferenceError.

## 3. Интеграционная проверка

- [x] 3.1 Запустить итоговый `make test` после всех изменений и убедиться, что проходят suites всех трёх targets; выполнить `openspec validate add-conj --strict` и проверить согласованность контракта, справочника и задач.
