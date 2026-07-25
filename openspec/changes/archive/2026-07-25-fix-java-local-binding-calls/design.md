## Context

Lowering преобразует обычные и destructuring `let` bindings в последовательные statement-формы `(let* name value)`. Для destructuring параметра функции это выглядит как fresh symbol parameter, временный binding исходного значения и leaf bindings через `get`.

Java generator начинает тело функции с параметрами в `ctx.locals`, но его body compilers вызывают `compile_expr` для каждой формы с одним и тем же контекстом. Singleton `let*` создаёт Java local variable, однако следующие формы не знают об этом symbol. В результате ветка generic call считает `(f value)` прямым Java method call вместо вызова локального `FnN`.

## Goals / Non-Goals

**Goals:**

- Последовательно расширять `ctx.locals` при компиляции lexical body.
- Компилировать RHS binding до добавления его имени в контекст и делать имя доступным только последующим формам того же блока.
- Использовать существующий dispatch локальных функций через `Fn0`-`Fn4`.
- Покрыть обычный и созданный destructuring local binding общим cross-backend sample.

**Non-Goals:**

- Менять lowering, AST, parser, eval или JavaScript generator.
- Добавлять новые runtime-интерфейсы или расширять максимальную Java function arity.
- Поддерживать recursive local bindings или вложенный `def`.

## Decisions

- Body compilation в Java generator будет обрабатывать формы слева направо и передавать обновлённый `ctx` следующей форме. При singleton `(let* name value)` RHS компилируется с текущим контекстом, после чего `name` добавляется в `ctx.locals`.
- Последовательный body compiler будет использоваться для top-level функций, Java lambdas, void Java lambdas и тел lexical blocks. Каждый вложенный блок начинает с унаследованного контекста, но не возвращает добавленные имена наружу.
- `compile_expr` сохранит текущий локальный call dispatch: atom из `ctx.locals` приводится к соответствующему `FnN` и вызывается через `.call(...)`; нелокальные runtime, top-level и qualified calls остаются прямыми.
- Предварительный сбор всех имён из тела отклонён: он сделал бы поздний binding видимым при компиляции более ранних RHS и нарушил последовательную семантику `let*`.
- Изменение lowering для сохранения исходных `let*` blocks отклонено как более широкое: текущая statement-нормализация используется обоими compiler targets и уже корректна для JavaScript.

## Risks / Trade-offs

- [Локальное имя может преждевременно попасть в контекст собственного RHS] → добавлять symbol только после компиляции RHS.
- [Binding из условной ветки может протечь в соседнюю ветку или внешний body] → обновлять контекст только внутри текущего lexical body и не возвращать его вызывающему block expression.
- [Разные body paths могут снова разойтись] → направить функции, lambdas и blocks через один последовательный механизм компиляции.
