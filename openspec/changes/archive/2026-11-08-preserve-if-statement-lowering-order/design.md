# Design

## Context

Мотивация — в `proposal.md`. Первый вариант statement lowering обнаружил перестановку эффектов, а вариант с consumer внутри value-let — захват поздних host-имён и изменение raw-code из-за переименования локальных переменных. Пользователь согласовал пересмотр после исследования и обсуждения пяти решений.

Java backend не располагает таблицей host-типов. Metadata синтаксически допустима перед произвольной формой, но существующий Java-контракт поддерживает type hints на bindings/параметрах, annotation lambda и явный cast, а не универсальный expression cast.

## Goals / Non-Goals

**Goals:** native statement-if, исходный порядок эффектов и lookup, исходные lexical scopes, совместимость корректных raw-code вставок и подготовка к отдельному change исключений.

**Non-Goals:** try/throw, новый runtime API, отдельный AST-анализ чистоты или host-типов, разбор/переписывание raw-code, изменения eval/FEX, автоматический вывод типа результата сложного выражения, универсальные expression type hints и распознавание Java-класса в receiver-позиции `.`.

## Decisions

### Обычные statements и temporaries

`lower_if` возвращает `(statements, result)`: fresh result declaration, condition prefix и native if; ветви вычисляют и присваивают выбранное значение. Discard-путь не требует result temporary. Generated имена создаются через Gensym.

Value-let вычисляет собственные bindings/body в lexical block, присваивает результат внешнему temporary и закрывает блок до consumer. Поздние операнды остаются в исходном окружении, включая имена из raw-code. Неаннотированный Java result temporary может иметь тип Object. Локальный var используется там, где тип доступен непосредственно из initializer.

Host receiver/callable, полученный из сложного выражения, пользователь типизирует на внешнем binding либо через cast. Например `(let [^String text (let [x (new String "a")] x)] (.concat text "b"))` возвращает `"ab"`. Тип внутреннего x автоматически наружу не переносится; `^String (let ...)` не становится новым универсальным cast.

Function/callback bodies потребляют statements перед return; JS definitions публикуют const/export const после prefixes, Java definitions используют static initializer с существующим sneaky_throw bridge для checked calls. Java void callback использует discard для всего тела; annotation не меняет value-семантику JS.

### Scope и raw-code

Инициализатор binding исполняется в предыдущем окружении, до открытия области нового имени. Последовательные bindings имеют последовательные области. JS использует обычные lexical blocks; Java переименование требуется только при реальном конфликте с уже активным local, не при затенении field/global и не для переноса consumer.

Текст raw-code остаётся непрозрачным и неизменным. Ранее корректные обращения к локальным переменным и выходящий Java return сохраняются. Границы веток и пользовательских callbacks сохраняются; deferred callback не исполняется при создании. Экспортируемый lower_binding остаётся совместим с FEX.

### Порядок операндов и target-specific вызовы

До prefixes позднего операнда сохранять ранние ещё не вычисленные значения ровно один раз. Типовые special forms отделяют вычисляемые значения от синтаксических имён.

В JS callee и constructor являются значениями, включая host-имена. При method/qualified вызове lookup метода происходит перед аргументами: сохранить receiver и функцию, затем выполнить аргументы и вызвать сохранённую функцию с исходным this через intrinsic apply (`[].map.apply.call`). Доступ через literal не зависит от затенённых Reflect/globalThis или собственного свойства apply у callee. Не проверять callable раньше аргументов. Для внутренних форм доступа к member и вызова с сохранённым receiver допустимы минимальные emitter markers в существующем sexpr-представлении; новый runtime helper не нужен.

В Java direct named/static call остаётся именем метода, local/dynamic callee — вычисляемым значением. Class/method является гарантированным static-синтаксисом; `.` означает instance receiver. Случайная работоспособность `(. String valueOf ...)` не является гарантией, диагностику такого случая отдельно не добавлять. Class/type/method в синтаксических позициях не материализуются как runtime-значения.

### Рассмотренные альтернативы

- Перенос consumer внутрь let сохраняет Java inference, но усложняет scope и захватывает неизвестные host-имена; отклонён.
- Java switch/yield сохраняет inferred тип без функции, но запрещает выходящий raw-code return; отклонён.
- Supplier/IIFE восстанавливают функцию-обёртку и мешают цели statement lowering; отклонены.
- Обычные blocks и Object-temporaries с явной типизацией host interop выбраны пользователем как минимальный контракт.

## Risks / Trade-offs

- [Ранее работавший inferred receiver за границей let потребует hint/cast] → явная migration-примечание и javac/JVM tests внешнего binding/cast, без нового общего expression hint.
- [Lookup или this сдвинется при materialization] → Node regressions getter, method replacement, host callee и constructor replacement, ранняя ошибка без поздних эффектов.
- [Scope/raw-code/callback capture] → общие shadowing tests и target-specific проверки локальных имён, host-имён, raw return и deferred callback.
- [Source maps] → metadata исходных вызовов и Node tests условия/ветки с картой.

## Migration Plan

Обновить имеющуюся незакоммиченную реализацию и tests по этому design, затем выполнить make test, review и strict OpenSpec validation. Вывод if_value.clj остаётся `7|12|3|nil|false|3|ab`. Коммит и архивирование — только по отдельному запросу; exceptions остаются в add-exception-handling.
