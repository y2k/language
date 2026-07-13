## Why

Существующий синтаксис `:import` принимает только один вектор классов, поэтому обычная группировка импортов из разных пакетов в одном namespace clause завершается ошибкой. Это вынуждает разносить импорты по нескольким `:import` clauses.

## What Changes

- Разрешить `:import` принимать один или несколько векторов импорта в одном clause.
- Сохранять существующую форму `(:import [package Class ...])` без изменения результата дешугорирования.
- Дешугорировать все классы из нескольких векторов в import pairs `compiler/ns` в исходном порядке.
- Добавить проверки дешугорирования и end-to-end Java sample, компилирующий импортированный класс через `javac`.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `frontend-syntax`: namespace macro должен принимать несколько векторов классов в одном `:import` clause.

## Impact

- `frontend/macro_ns.ml`: разбор `:import` clause.
- `test/frontend_desugar_test.ml`: проверка формы `compiler/ns` для нескольких векторов.
- `test/samples/java/`: интеграционный sample с импортом Java-класса.
- Java-генератор, публичный синтаксис существующих импортов и зависимости не меняются.
