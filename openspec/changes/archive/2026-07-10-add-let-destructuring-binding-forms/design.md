## Context

Сейчас `let` macro только переименовывает форму в `let*`, а `let*` в eval и compiler targets принимает binding pairs вида `name value`. Eval уже содержит близкую логику `bind_param` для destructuring параметров функций: атом связывается напрямую, `(list ...)` разбирает runtime list.

## Goals / Non-Goals

**Goals:**

- Разрешить в `let`/`let*` binding pattern вместо простого имени.
- Поддержать простые symbol bindings, sequential patterns и associative/map patterns.
- Разрешить вложенные patterns в sequential и associative destructuring.
- Сохранить последовательное вычисление binding values.
- Добиться одинакового результата на `eval`, `js` и `java` targets.

**Non-Goals:**

- Полная совместимость с Clojure destructuring sugar: `:keys`, `:strs`, `:syms`, `:or`, `&`, `:as`.
- Новый синтаксис parser/AST.
- Поддержка nested `def` или изменение top-level convention.

## Decisions

- Переиспользовать текущую s-expression форму patterns вместо нового AST. Sequential pattern после macro expansion уже представлен как `(list ...)`, associative pattern как `(hash-map key binding ...)`.
- В eval обобщить binding логику до функции вида `bind_pattern`, чтобы `fn*` параметры и `let*` bindings шли через один код там, где это возможно.
- Для `let*` вычислять правую часть один раз, затем применять pattern к полученному значению и расширять environment перед следующим binding.
- Для associative destructuring читать значения по ключам из runtime hash map; отсутствующий ключ связывает `nil`, как `get` для hash map.
- Lowering должен рекурсивно заменить каждый destructuring pattern на обычные symbol bindings. Для destructuring value он сначала создаёт временное имя через `Gensym`, затем связывает leaf symbols с core-вызовами `(get temporary selector)`. Это избегает повторного вычисления RHS и сохраняет порядок.
- После lowering левый операнд каждого `let*` должен быть только symbol. JavaScript и Java generators не распознают destructuring patterns и компилируют результат lowering без специальной логики.
- `get` является target-neutral core primitive: JavaScript и Java runtimes должны реализовать его для list по числовому индексу и hash-map по ключу. Отсутствующий ключ или индекс возвращает `nil`/`null`, как в evaluator.

## Risks / Trade-offs

- Разные runtime representations коллекций в eval/js/java могут разойтись -> покрыть одинаковым sample/focused tests на всех targets.
- Ошибки destructuring могут иметь разные строки в targets -> специфицировать поведение по смыслу, а не по точному тексту, кроме уже существующих eval errors.
- Отложенный Clojure sugar может понадобиться пользователям -> добавить отдельным change, когда появятся реальные примеры.
