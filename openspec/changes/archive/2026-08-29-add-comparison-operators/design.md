## Context

`Symbol_munge.munge` уже преобразует `>` и `<` в `_GT_` и `_LT_`; составные symbols `>=` и `<=` становятся `_GT__EQ_` и `_LT__EQ_`. JavaScript generator компилирует такие forms как обычные function calls, но его явный runtime import не предоставляет эти identifiers. Java generator использует wildcard static import, однако Java runtime не содержит соответствующих methods. Eval stdlib также не регистрирует исходные operator symbols.

Файлы `prelude/language_runtime.js` и `prelude/language_runtime.java` являются links на `prelude/1.0.0` в соседнем репозитории `packages`. Предыдущие обратно совместимые additions, включая `_EQ_` и `drop`, обновляли эту версию на месте.

## Goals / Non-Goals

**Goals:**

- Использовать существующий путь generic function call и runtime helpers без новой compiler form.
- Сохранить одинаковый подтвержденный результат четырех сравнений на `eval`, `js` и `java` для поддерживаемых integer значений.
- Доставить compiler и runtime изменения согласованно между репозиториями.

**Non-Goals:**

- Добавлять variadic comparison semantics или проверять arity.
- Определять поведение для non-integer operands и единый диапазон integer между targets.
- Поддерживать first-class comparison operators, shadowing или redefinition их names.
- Вводить selective imports, центральный registry builtins или новую версию package.

## Decisions

- Реализовать по одному binary helper для каждого munged name: `_GT_`, `_LT_`, `_GT__EQ_` и `_LT__EQ_`. JavaScript и Java используют native integer comparisons, evaluator преобразует operands существующим integer conversion path и возвращает обычное runtime boolean value. Альтернатива с frontend macro или inline compiler operator отклонена, поскольку создала бы отдельный compilation path вместо устранения отсутствующих runtime functions.
- Добавить все четыре names в существующий unconditional JavaScript runtime import. Selective import потребовал бы анализа qualified names и scopes, не нужного для этого изменения.
- Обновить `prelude/1.0.0/js/language_runtime.js` и `prelude/1.0.0/java/language_runtime.java` на месте. Addition новых helpers обратно совместим и следует существующему package precedent; version bump потребовал бы вручную менять absolute links без пользы для поддерживаемого контракта.
- Проверить contract одним общим sample, который содержит true и false case для каждого оператора. Sample harness уже исполняет общий fixture через evaluator, Node и Java, поэтому отдельные target-specific tests не нужны.
- Не добавлять tests или validation для wrong arity, non-integer operands, shadowing и больших platform-dependent integer. Эти случаи намеренно не имеют гарантированного поведения.

## Risks / Trade-offs

- [Старый JavaScript runtime не экспортирует новые unconditional imports] -> Сначала обновить `prelude/1.0.0`, затем compiler import; публиковать изменения согласованно.
- [Поведение unsupported operands различается между targets] -> Не заявлять и не тестировать это поведение до отдельного решения о numeric model.
- [Unconditional import усиливает связь generated modules с runtime version] -> Сохранить текущую простую import architecture; вернуться к selective imports только при подтвержденной необходимости version coexistence.
- [Имена `_GT__EQ_` и `_LT__EQ_` выглядят необычно] -> Использовать существующий детерминированный symbol munging вместо второго naming convention.

## Migration Plan

1. Добавить четыре helpers в JavaScript и Java runtimes `prelude/1.0.0` в репозитории `packages`.
2. Добавить eval bindings и четыре JavaScript imports в репозитории `language`.
3. Добавить общий sample и выполнить `make test` с обновленными linked runtimes.

При rollback сначала удалить новые identifiers из JavaScript compiler import и eval bindings. Additive runtime helpers можно оставить; если их требуется удалить, это безопасно после отката compiler.
