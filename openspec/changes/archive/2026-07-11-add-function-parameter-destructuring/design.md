## Context

Frontend macros already preserve function parameter patterns: `[a b]` becomes `(list a b)` and `{:name n}` becomes `(hash-map "name" n)`. The eval backend accepts only sequential patterns for function parameters. Compiler lowering eliminates patterns only from `let*` bindings, while JavaScript and Java generators require every `fn*` parameter to be a symbol.

The existing `let*` implementation is the target semantic model: sequential and associative patterns can be nested; missing selected values become `nil`/`null`; and a destructured value is evaluated once.

## Goals / Non-Goals

**Goals:**

- Сделать patterns параметров функций одинаково доступными в eval, JavaScript и Java.
- Поддержать symbol, sequential, associative и nested patterns.
- Сохранить arity функции: один top-level pattern соответствует одному аргументу.
- Использовать existing `let*` lowering и `Gensym`, не добавляя destructuring в generators.
- Привести eval к `let*`-семантике отсутствующих list items и hash-map keys.

**Non-Goals:**

- Полная Clojure destructuring syntax: `:keys`, `:strs`, `:syms`, `:or`, `&` и `:as`.
- Новый parser, AST или runtime helper.
- Изменение arity checks для вызова функций.
- Изменение поведения `let*` destructuring.

## Decisions

- Перед lowering заменять каждый non-symbol параметр свежим symbol из `Gensym`, а исходные parameter patterns добавлять как initial `let*` bindings в тело функции. Затем existing lowering рекурсивно устраняет эти bindings через `get`.
  - Это оставляет JavaScript и Java generators с их текущим contract: параметры только symbols.
  - Alternative: реализовать patterns отдельно в `js.ml` и `java.ml`. Отклонено: дублирует traversal, локальные имена и target semantics.
- Разрешить associative patterns в eval function parameters и отключить exact list-length validation для параметров. Missing list index и map key связываются с `nil`, а extra list items игнорируются.
  - Это делает eval результат совместимым с compiler lowering, где `get` уже возвращает `nil`/`null` для отсутствующего индекса или ключа.
  - Alternative: сохранить exact list length. Отклонено: потребует нового target-neutral runtime validation path и расходится с `let*`.
- Сохранять проверку внешнего arity до destructuring: число переданных аргументов должно совпадать с числом top-level параметров.
- Тестировать public forms `fn` и `defn` на всех трёх targets, а также direct `fn*` в focused eval test для core behavior.

## Risks / Trade-offs

- [Изменяется существующее eval behavior для list patterns неверной длины] → Зафиксировать новую `let*`-совместимую семантику в delta spec и regression tests.
- [Generated wrapper может изменить порядок body expressions] → Добавлять bindings только в начало тела и использовать existing lowering body path; покрыть многоформное тело.
- [Ошибки неверного типа collection различаются между runtimes] → Специфицировать успешное binding behavior, а не точный текст target errors.
- [Java поддерживает ограниченный function arity] → Не менять число top-level параметров; existing Java arity limitation остаётся в силе.
