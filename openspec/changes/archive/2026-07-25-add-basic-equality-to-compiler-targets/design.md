## Context

Frontend уже преобразует `=` в identifier `_EQ_`. JavaScript generator использует явный runtime import, Java generator использует wildcard static import, но связанный versioned пакет `prelude` пока не определяет `_EQ_` ни для одной платформы. Evaluator имеет отдельную variadic и structural реализацию, которую это изменение не затрагивает.

## Goals / Non-Goals

**Goals:**

- Сделать вызов `(= left right)` исполняемым в JavaScript и Java.
- Обеспечить одинаковые результаты для двух scalar-значений одного поддерживаемого типа.
- Проверить доступность и поведение через общий sample harness.

**Non-Goals:**

- Поддерживать ноль, один или более двух аргументов на compiler targets.
- Определять equality для collections, functions или mixed numeric types.
- Менять evaluator и его модель runtime values.

## Decisions

- JavaScript `_EQ_` использует `Object.is(left, right)`. По сравнению с `===` это ближе к Java `Objects.equals` для `NaN` и signed zero, хотя mixed numeric behavior остаётся вне scope.
- Java `_EQ_` использует `java.util.Objects.equals(left, right)`, не добавляя собственную comparison abstraction.
- JavaScript generator добавляет `_EQ_` в существующий явный runtime import; Java generator менять не требуется благодаря wildcard static import.
- Общий sample проверяет только согласованное подмножество evaluator, JavaScript и Java: равные и неравные значения одного scalar-типа.
- Вызовы с арностью, отличной от двух, не входят в контракт этой версии; отдельная runtime-валидация для них не добавляется.

## Risks / Trade-offs

- JavaScript принимает лишние аргументы и подставляет `undefined` для отсутствующих → такие вызовы явно остаются вне контракта до решения общей arity semantics.
- `1` и `1.0` неразличимы как JavaScript `Number`, но могут иметь разные Java wrappers → mixed numeric types исключены из требований и тестов.
- Evaluator поддерживает более широкую variadic/structural семантику → proposal не заявляет полную cross-target parity за пределами двух scalar-аргументов.
