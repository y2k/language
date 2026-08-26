## Context

См. мотивацию в `proposal.md`. `meta` сейчас содержит только source location и type annotation. `defn` и `defn-` оба раскрываются в `def`, а desugar применяет не более одного macro к outer form. Поэтому private intent теряется до JS generation, а исходный `def-` остаётся вызовом обычной функции.

## Goals / Non-Goals

**Goals:**

- Сохранить private intent от исходных `def-` и `defn-` до JS generation.
- Сделать public top-level definitions ESM exports без изменения eval и Java core-semantics.
- Разрешить цепочку `defn-` -> `defn` -> `def` -> `fn*` через общий механизм macro expansion.

**Non-Goals:**

- Не добавлять отдельную core-форму `def-`.
- Не менять доступность Java или eval definitions.
- Не менять форму JavaScript functions с arrow expression на function declaration.

## Decisions

### Хранить private flag во внешней metadata definition

Добавляется boolean `private_` в `Frontend.meta` с default `false`. Macros создают обычный `def` и устанавливают `private_ = true` у metadata внешнего `SList`; последующие expansions сохраняют этот record.

Внешний `SList` описывает declaration целиком и уже сохраняется lowering. Metadata атома `def` не используется для определения формы, поэтому не подходит для свойства declaration.

Альтернативы:

- Отдельная core-форма `def-` потребовала бы поддержки в lowering, eval и Java, хотя private visibility нужна только JS.
- Отдельное поле у AST `sexpr` расширило бы представление сильнее, чем нужно; свойство уже относится к metadata формы.

### Повторно раскрывать outer form до отсутствия matching macro

Desugar разделяет поиск одного раскрытия и рекурсивный обход. Если macro вернул replacement, replacement снова подаётся в поиск macro; дочерние формы обходятся только после завершения цепочки. Это позволяет `defn-` заменять на private `defn`, не дублируя логику `defn` и `fn` macros.

Альтернатива, в которой `defn-` напрямую создаёт private `def`, дублирует устройство `defn` и перестаёт быть композиционным при развитии macro expansion.

### Генерировать export только по private metadata

JS compiler генерирует `export const` для обычного top-level `def` и `const` для private `def`. Function definitions остаются bindings к arrow expressions, поэтому `defn` экспортируется как `export const`, а `defn-` остаётся локальным `const`.

Это сохраняет существующий JS representation functions и делает public/private distinction ровно в target, где она наблюдаема.

## Risks / Trade-offs

- [Macro, который бесконечно раскрывается в другую matching form] -> Built-in macros должны уменьшать или устранять macro syntax; регрессионные tests покрывают завершающуюся цепочку `defn-`.
- [Изменение public JS output] -> Обновить точные JS compiler expectations и явно проверить все четыре declaration forms.
- [Private flag потерян при создании replacement] -> Проверить frontend AST после каждой цепочки и JS output для private forms.

## Migration Plan

1. Обновить compiler snapshots и sample expectations для public ESM exports.
2. Сгенерированные JS-модули начнут экспортировать public definitions; consumers должны импортировать их как ESM bindings.
3. Откат состоит в возврате прежнего JS generation и удалении private metadata support; persisted data и runtime migration отсутствуют.
