## Context

`backend_eval/eval_stdlib.ml` уже различает формы `(reduce fn collection)` и `(reduce fn init collection)`. В `prelude/language_runtime.js` и `prelude/language_runtime.java` реализована только первая форма, хотя generators передают аргументы runtime-вызовам без преобразования арности.

## Goals / Non-Goals

**Goals:**

- Обеспечить одинаковую left-fold семантику трёхаргументного `reduce` на `eval`, `js` и `java` targets.
- Вернуть `init` без вызова `fn`, если список пуст.
- Не изменить поведение существующей двухаргументной формы.

**Non-Goals:**

- Поддержка hash-map в JavaScript и Java runtimes.
- Изменения parser, desugaring или compiler generators.
- Добавление новых collection abstractions или зависимостей.

## Decisions

- Расширить существующие runtime-функции вместо изменения generators: generated calls уже сохраняют исходное число и порядок аргументов.
- В JavaScript различать формы по фактическому числу аргументов, а не по значению `init`, поскольку `nil` является допустимым initial value.
- В Java добавить overload `reduce(Object fn, Object init, Object collection)` рядом с существующей двухаргументной функцией. Это сохраняет текущий API и использует обычное разрешение overload при компиляции generated Java.
- Трёхаргументная форма обходит список с первого элемента и применяет `fn(acc, item)` к каждому элементу. Двухаргументная форма продолжает брать первый элемент как accumulator и отклонять пустой список.
- Проверить контракт одним общим sample, который запускается всеми тремя targets и покрывает непустой и пустой список. Существующий `list_map_reduce.clj` остаётся проверкой двухаргументной формы.

## Risks / Trade-offs

- [Различия arity-dispatch между JavaScript и Java] → Закрепить одинаковое наблюдаемое поведение cross-backend sample-тестом.
- [Случайное принятие `init` за collection в JavaScript] → Выбирать форму по числу аргументов, а не по типу или значению аргумента.
- [Ожидание parity для hash-map из-за eval] → Явно ограничить требование списками; поддержку hash-map оформить отдельным изменением.
