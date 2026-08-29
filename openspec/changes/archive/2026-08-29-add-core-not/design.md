## Context

См. `proposal.md` для мотивации и delta specs для требуемого поведения. Все backends уже компилируют `(not value)` по generic function-call path. JavaScript generator получает runtime-функции через явный unconditional import, Java generator использует wildcard static import, а evaluator разрешает обычные имена через stdlib после local и namespace bindings.

Language truthiness уже реализована отдельно каждым target: falsey являются `false` и `nil`, тогда как такие JavaScript falsey values, как `0` и пустая строка, в языке truthy. Файлы `prelude/language_runtime.js` и `prelude/language_runtime.java` являются links на `prelude/1.0.0` в соседнем репозитории `packages`.

## Goals / Non-Goals

**Goals:**

- Сохранить generic function-call compilation и добавить только отсутствующие runtime bindings.
- Получить одинаковое подтверждённое truthiness behavior на `eval`, `js` и `java`.
- Сохранить возможность lexical binding с именем `not` перекрывать stdlib/runtime name там, где существующий backend поддерживает такое shadowing.

**Non-Goals:**

- Вводить новую compiler form, frontend macro, selective JavaScript imports или общий registry builtins.
- Добавлять compile-time resolution неизвестных function names.
- Расширять существующую поддержку first-class runtime functions в Java.
- Определять cross-target error contract для неправильной arity.

## Decisions

- Реализовать `not` как обычную runtime-функцию каждого target. Frontend macro `(not value) -> (if value false true)` отклонён, потому что зарезервировал бы имя на macro expansion и перехватывал вызовы lexical function с именем `not`. Отдельный JS compiler case отклонён из-за target-specific compilation path.
- Вычислять результат через существующую truthiness semantics target, а не через native JavaScript `!value`. Native negation дала бы неверный результат для `0` и других значений, которые falsey в JavaScript, но truthy в языке.
- Добавить `not` в существующий unconditional JavaScript runtime import. Selective import потребовал бы анализа вызовов и scopes без пользы для текущей простой runtime architecture. Java compiler менять не требуется благодаря wildcard static import.
- Обновить `prelude/1.0.0/js/language_runtime.js` и `prelude/1.0.0/java/language_runtime.java` на месте. Добавление helper обратно совместимо и следует существующему package precedent для `_EQ_`, comparisons и `drop`.
- Проверить контракт одним общим sample с falsey cases, truthy `0` и исходной формой `(not (= value 0))`. Существующий sample harness исполняет fixture через evaluator, Node и Java, поэтому отдельные backend execution tests не нужны.

## Risks / Trade-offs

- [Generated JavaScript начнёт импортировать `not` из runtime, где старые версии его не экспортируют] -> Сначала обновить linked runtime `prelude/1.0.0`, затем compiler import и поставлять изменения согласованно.
- [Unconditional import увеличивает список bindings во всех generated modules] -> Сохранить существующую простую import architecture; обновить текущие snapshots без добавления selective import machinery.
- [Truthiness implementations targets могут разойтись позже] -> Cross-target sample закрепляет `false`, `nil` и `0` как минимальные различающие cases.

## Migration Plan

1. Добавить `not` в JavaScript и Java runtimes `prelude/1.0.0` репозитория `packages`.
2. Добавить eval binding и JavaScript runtime import в репозитории `language`.
3. Добавить общий sample и выполнить `make test` с обновлёнными linked runtimes.

При rollback сначала удалить `not` из JavaScript compiler import и eval stdlib. Additive runtime helpers можно оставить; их удаление безопасно после отката compiler import.
