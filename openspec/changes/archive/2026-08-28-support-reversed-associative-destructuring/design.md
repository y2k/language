## Context

См. `proposal.md` для мотивации и `specs/frontend-syntax/spec.md` для контракта. Parser сохраняет brace/bracket формы и исходный atom `:keyword`, но общий `keyword_macro` затем превращает keyword в строку. Eval и compiler lowering получают уже раскрытый associative pattern и независимо ожидают canonical пары `(hash-map key binding ...)`, поэтому после keyword expansion надёжно определить reverse-порядок нельзя.

`let_macro` и `fn_macro` являются последними общими точками, где известен binding-контекст и ещё сохранены raw brace-patterns. `defn` и `defn-` уже раскрываются через `fn`.

## Goals / Non-Goals

**Goals:**

- Нормализовать reverse keyword-пары до generic collection и keyword expansion.
- Сохранить единое canonical представление patterns для eval и compiler lowering.
- Разместить распознавание рядом с macros, которые определяют binding-позиции.

**Non-Goals:**

- Поддержка reverse-порядка для строковых или символьных selectors.
- Нормализация прямых `let*`/`fn*` и явно записанных `(hash-map ...)` patterns.
- Новая валидация malformed или несимвольных leaf bindings.
- Полная Clojure destructuring syntax, включая `:keys`, `:or`, `:as` и `&`.

## Decisions

### Использовать общий helper рядом с let_macro и fn_macro

В `frontend/builtin_macros.ml` будет один рекурсивный helper для raw binding patterns. `let_macro` применит его только к pattern в каждой паре `pattern/value`, не посещая RHS и body. `fn_macro` применит его к каждому parameter pattern до существующей обработки type annotations. Это автоматически покрывает `defn` и `defn-` без отдельной логики.

Альтернатива с отдельной context-aware фазой в `frontend/desugar.ml` отклонена: она расширяет центральный macro traversal ради двух известных binding forms. Дублирование helper в обоих macros отклонено из-за рекурсивной обработки вложенных patterns.

### Определять reverse-пару по raw keyword справа

Для brace pattern helper обрабатывает элементы попарно. Если второй элемент является atom, который распознаётся существующим правилом `keyword_macro`, пара `binding :keyword` переставляется в `:keyword binding`, а исходный binding рекурсивно нормализуется. Иначе порядок сохраняется, а рекурсивно нормализуется существующая binding-сторона пары. Bracket pattern рекурсивно нормализует каждый item; остальные формы остаются без изменений. Перестановка переиспользует исходные AST nodes и их metadata.

Смешанная ориентация, пара из двух keywords и malformed нечётный список не получают отдельной семантики или новой диагностики. Helper применяет локальное правило к распознаваемым парам и оставляет остальные элементы существующему pipeline.

Альтернатива с backend heuristic отклонена: после `keyword_macro` source keyword неотличим от строкового литерала, а исправление пришлось бы дублировать в eval и compiler lowering. Новый keyword/pattern AST отклонён как несоразмерное изменение для compatibility alias.

### Ограничить нормализацию пользовательскими brace-patterns

Helper вызывается только во время раскрытия outer `let` и `fn` и распознаёт raw `Brace`/`Bracket`. Обычные map-выражения, прямые core-формы `let*`/`fn*` и явные `(hash-map ...)` patterns не затрагиваются. После нормализации существующие `hash_map_macro` и `keyword_macro` создают canonical `(hash-map "key" binding ...)`, поэтому backend-код не меняется.

## Risks / Trade-offs

- [Нормализация случайно переставит обычный map literal] -> Вызывать helper только для pattern slots, не обходить RHS и body.
- [Вложенный reverse pattern останется ненормализованным] -> Рекурсивно обходить binding-сторону brace-пар и каждый item bracket-pattern.
- [Eval, JavaScript и Java разойдутся по поведению] -> Выполнить один общий sample через существующий cross-target harness.
- [Ограничение только `let`/`fn` удивит пользователей прямых core-форм] -> Явно закрепить границу в spec; расширять core-семантику только отдельным изменением при реальной необходимости.

## Migration Plan

Миграция данных и runtime не требуется. После выпуска существующие key-first patterns сохраняют поведение, а опубликованные packages с reverse keyword-парами начинают работать. Откат состоит в возврате frontend helper и regression tests; canonical backend representation не меняется.
