## Context

Parser уже сохраняет `^TYPE` в `meta.type_annotation` следующего символа. `let` macro переносит binding-символ без замены, а compiler lowering сохраняет этот символ при преобразовании binding-вектора в singleton `(let* NAME VALUE)` statement. Java generator сейчас игнорирует metadata имени и генерирует `var NAME = VALUE`; если `VALUE` имеет статический Java-тип `Object`, Java не может разрешить вызов метода, указанный type hint не используется.

## Goals / Non-Goals

**Goals:**

- Использовать type hint символьного `let` binding при генерации Java local variable.
- Сохранить однократное и последовательное вычисление RHS.
- Проверить полный Java-путь через существующий `javac`/`java` sample harness.

**Non-Goals:**

- Менять parser, AST, `let` macro или compiler lowering.
- Придавать type hints runtime-семантику в eval или JavaScript targets.
- Поддерживать type hints на destructuring patterns.
- Валидировать имя Java-типа до запуска `javac`.

## Decisions

### Обрабатывать hint в Java generator

Ветка Java generator для singleton `(let* NAME VALUE)` прочитает `type_annotation` из metadata `NAME`. При наличии hint скомпилированный RHS будет обёрнут Java cast и останется initializer для `var`, концептуально: `var name = ((TYPE) value);`. Это использует уже существующую Java cast-семантику и совпадает с формой локальных bindings, создаваемых для аннотированных параметров функций.

Альтернатива с преобразованием hinted `let` в `(cast TYPE VALUE)` во frontend macro отклонена: она без необходимости меняет AST для eval и JavaScript, хотя требование относится только к Java target.

### Ограничить поддержку символьными bindings

Hint применяется только к `SAtom` слева от binding. Lowering уже гарантирует symbol-only bindings для generators и сохраняет metadata исходного leaf symbol. Отдельная семантика для аннотированных sequential или associative destructuring patterns не вводится.

### Использовать Java-only end-to-end sample

Fixture в `test/samples/java/` получит значение из функции языка с Java return type `Object`, свяжет его с `^java.lang.String` и вызовет метод `String`. Без cast sample обязан падать на `javac`; после изменения он компилируется и возвращает ожидаемую строку. Generated-source unit test не добавляется, поскольку end-to-end sample напрямую проверяет требуемое поведение.

## Risks / Trade-offs

- [Metadata binding-символа может быть потеряна при будущих изменениях lowering] → Java-only sample проверяет весь путь от source syntax до `javac` и выполнения.
- [Ошибочный или несовместимый `TYPE` приводит к Java compile error] → Сохранить обычную диагностику `javac`; отдельная система Java type validation не нужна.
- [Cast может завершиться `ClassCastException` во время выполнения] → Это ожидаемая Java-семантика неверного type hint и не требует runtime-обвязки.
