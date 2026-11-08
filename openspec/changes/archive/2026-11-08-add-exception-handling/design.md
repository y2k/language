# Design

## Context

Мотивация и границы согласования — в `proposal.md`; контракт — в трёх delta specs. Предварительное условие реализации — завершённый `preserve-if-statement-lowering-order`: общий переход if на statements/value, порядок операндов и body/initializer consumers принадлежат ему. Здесь этот механизм расширяется для try и проверяется в защищённых секциях. Описание ниже фиксирует архитектурные требования интеграции, а не повторное поручение общего рефакторинга. Исследованная исходная база — `5b012b22ac9fc2485eaeeb611f89903c2c3b4937`; история чата и отменённый код не являются зависимостями. Новые exception helpers и fixtures требуется создать.

Наблюдаемая базовая реализация:
- `frontend/ast.ml` уже хранит формы как s-expressions с metadata; новый reader или AST variant не нужен. `frontend/desugar.ml` рекурсивно раскрывает содержимое форм.
- `backend_compiler/lowering_expression_to_statement.ml` имеет `lower_value` с парой `(statements, value)`, `lower_discard`, `lower_body_value`, `lower_body_discard`, `statement_block`, `Gensym`. `lower_if` создаёт временную переменную и `set!`, но возвращает их внутри value-`do`. `lower_expr` тоже упаковывает statements в `do`. Это ещё не полное устранение wrappers: `if` в аргументе/RHS остаётся expression-`do` для генератора.
- `lower_values` конкатенирует префиксы до остаточных выражений всех аргументов. Если начать выносить `try` таким способом, более ранний вызов может исполниться после более позднего `try`. Generic обход списка также не различает вычисляемые операнды и синтаксические имена interop.
- `backend_compiler/js.ml` уже использует IIFE для expression-valued тел и умеет генерировать инструкции с source-map metadata.
- `backend_compiler/java.ml` использует typed `Supplier<Object>` для `do` в позиции значения; методы и `Fn0`–`Fn4` допускают `Exception`. Typed callbacks и `gen-class` используют `sneaky_throw`, который сохраняет исходный `Throwable`.
- Для сложного top-level `def` Java уже создаёт поле и `static { ... }`, но внутри блока присваивает результат `compile_expr`. JS печатает `export const name = expression`. Оба entry point нужно научить потреблять statement prefix и затем публиковать результат.
- `backend_eval/eval.ml` имеет явные отказы для target-specific форм; `backend_compiler/fex.ml` — список неподдерживаемых expression forms. FEX сериализует собственное подмножество без общего JS/Java lowering.
- `test/test.ml` запускает целевые samples на Node/JVM; `test/raw_code_test.ml` показывает принятый способ проверки diagnostics и eval rejection; `test/lowering_test.ml` покрывает структуру преобразований.

## Goals / Non-Goals

**Goals:**
- Использовать native host exception unwinding и существующие средства компилятора для value/discard контекстов.
- Получать native `try` на том же уровне исполнения, где находится исходная форма: никаких дополнительных `Supplier`/IIFE/callback/helper-методов ради значения `try`, включая вложенность в `do`, `let`, `if`, вызовы и top-level инициализаторы.
- Сохранить области исполнения, исходные объекты ошибок и metadata без нового прохода анализа исключений.
- Сделать fixtures исполняемыми на обоих compiled targets, а eval/FEX явно ограничить.

**Non-Goals:**
- Новая иерархия runtime-ошибок, Result-протокол, автоматическое подавление/агрегирование исключений.
- Полная замена IR, CPS/CFG/SSA, отдельный проход анализа исключений или terminating paths, функция открытия ресурсов или изменение guest FEX interpreter.
- Удаление всех старых wrappers из несвязанных программ и измерение производительности JVM. Локальные изменения общего lowering и lexical scope, необходимые для новых форм и порядка эффектов, входят в scope.
- Реальные Android-сборки или редактирование внешнего REPL в этом change.

## Decisions

### Approaches

Выбран expression API `try`/`catch`/`finally` и `throw` с общим statement lowering. `try` передаёт результат временной переменной; генераторы печатают native statements. Альтернатива с `Supplier`/IIFE проще для генератора выражений, но обходит существующий lowering и добавляет функцию ради control flow; она отклонена, даже если execution tests проходят. Runtime callbacks (`protect`/handler) и Result-протокол также не выбраны: они меняют API и не решают задачу обычного последовательного кода с host exceptions. Новый IR/CFG ради этой задачи избыточен.

### Разбор структуры при существующем lowering

Распознавать `try` и `throw` до generic-list ветвей `lower_value` и `lower_discard`. Разбирать хвостовые секции и проверять согласованную грамматику в общем compiler helper, вызываемом из этого же этапа. Не добавлять предварительный обход AST ради диагностики. `quote` уже останавливает lowering и должен продолжать это делать.

Создать небольшой общий parser секций (например, `backend_compiler/exception_forms.ml`), возвращающий тело, необязательный handler с binding и cleanup. Сохранять lowered `try` как структурную statement-форму для обоих генераторов. Не пропускать `catch`/`finally` через generic вызов. Проверку symbol binding использовать по существующему образцу проверок binding syntax; не расширять grammar destructuring.

Frontend grammar специфицируется как пользовательский синтаксис, но проверка выполняется при JS/Java compilation, не требует нового parser или глобального macro validator. Корректные `let`, `fn`, interop и другие макросы внутри секций проходят обычный desugar.

### Statement lowering в value и discard контекстах

Для value-`try` общий lowering возвращает пару: prefix с объявлением fresh result и native `try`; value — символ result. Каждое тело понижается отдельно: предыдущие формы в discard-контексте, последняя — в значение и присваивание result. Cleanup всегда discard. Объявление result с `nil` до `try` допустимо для Java typing/definite assignment; оно не вычисляет пользовательский эффект. Никакие вычисления тела/handler/cleanup не выходят из соответствующей секции.

Схема для `(consume (try (operation) (catch error (recover error)) (finally (cleanup))))`:

```java
Object result = null;
try {
    result = operation();
} catch (Exception error) {
    result = recover(error);
} finally {
    cleanup();
}
return consume(result);
```

Имена здесь иллюстративные; generated names создавать через `Gensym`. `finally` не содержит сгенерированного присваивания result или `return`. Не сохранять значение через truthiness и не дублировать cleanup по путям. Нормальные `nil`/`false` сохраняются.

Discard-`try` содержит только native statement: никаких result temporaries, фиктивного последнего `nil` или вызова wrapper. Это необходимо и для непоследней формы функции, и для финальной формы Java void-callback. Значимые значения в discard-контексте нормализовать существующим способом, чтобы Java не получал `1;`, `false;` или `return voidCall();`.

### Доставка statements до места исполнения

Переиспользовать пару `(statements, value)` до ближайшего существующего statement body. Не заворачивать lowered `try` обратно в expression-`do`, который попадёт в `compile_expr` и восстановит `Supplier`/IIFE.

- Тела `defn` и обычных `fn`: предыдущие формы — discard; prefix последней формы — statements перед итоговым `return value`.
- Java typed callbacks: value-режим аналогичен обычной функции; void-режим отбрасывает все результаты, включая последний. Учет `^void:` target-specific: та же metadata не меняет value-семантику JS lambda. Сохранить существующие checked-exception bridges typed callbacks/gen-class.
- Вложенные `do`, `let`, `if`: передавать prefix наружу лишь до текущего допустимого statement body, не через ветку, функцию или защищённую секцию. Путь через `lower_if` также должен выдавать statements и result, когда иначе он обернул бы вложенный `try`. Невыбранная ветка не исполняется.
- Java `def`: emitted statements идут в существующий `static` block перед присваиванием поля. Checked exceptions на границе static initialization передаются через существующий `sneaky_throw` в обычном `catch`, не через lambda. Native правила Java class initialization остаются в силе.
- JS `def`: prefix исполняется на уровне модуля перед `export const` с итоговым значением; сохранить порядок инициализации определений и visibility. Для отдельной top-level формы prefix предшествует её остаточному выражению.
- Реальные пользовательские functions/callbacks остаются lexical границами: их тела нельзя вынести в момент создания closure. Fresh result mutable внутри своего тела; захватываемые пользовательским callback bindings должны оставаться корректными для Java effectively-final правил.

Отдельный backend-local обход AST для этого не нужен. Расширять существующие lowering/body consumers; не создавать runtime `try` helper. Экспортируемый `lower_binding`, используемый FEX, сохранить совместимым.

### Порядок вычисления операндов

В `(f (before) (try (work) (finally (clean))) (after))` порядок — `before,work,clean,after,f`. При ошибке `before` защищённая операция ещё не начинается; при необработанной ошибке `work` нет `after` и вызова `f`. Внешний catch должен видеть обе ошибки, внутренний — только ошибки своего тела.

Перед prefix более позднего операнда материализовать необходимые более ранние вычисляемые значения ровно один раз во fresh bindings. Сохранять порядок вычисления dynamic callee, receiver и аргументов, а также значений коллекций. Не превращать в runtime bindings имена методов, классов, static qualifiers, `cast`/`instance?` type names или другие синтаксические позиции. При materialization сохранять Java type annotations/тип receiver и callable, а не стирать всё до `Object`.

Использовать существующие ветви для специальных форм/interop; не добавлять анализ чистоты или универсальную классификацию AST ради оптимизации числа temporaries. Выполнение prefix остаётся внутри исходной ветки, callback или защищённой секции.

### Lexical scope и native exceptions

Java handler имеет native тип `Exception`, что позволяет напрямую вызвать `.getMessage` и передать объект host logger. При совпадении имени с внешним local Java запрещает redeclaration: использовать fresh catch identifier через `Gensym` и scoped разрешение пользовательского имени в контексте генерации. Это же соответствие должно применяться внутри вложенных функций обработчика и сниматься при выходе из него; новые внутренние bindings затеняют его обычным образом. Не делать отдельный AST rename pass и не менять общую модель shadowing несвязанных программ.

Не flatten-ить вложенный `let` в scope catch: `(catch e (let [e replacement] (str e)) (throw e))` обязан повторно бросить пойманную ошибку. Statement block должен давать реальные `{ ... }` на обоих targets; два соседних `(let [x ...] ...)` не конфликтуют. Инициализатор shadowing binding вычисляется в предыдущем окружении; его имя действует только в собственном теле. Контекст генератора восстанавливается после блока, а тип catch binding остаётся `Exception`.

JS использует native catch с исходным значением, включая falsey values. В обоих targets native `finally` определяет приоритет ошибки очистки. Не добавлять `cause` или suppressed exceptions. JS source map использует существующие `at_form`/metadata, сохраняя позиции пользовательских вызовов внутри секций.

### Throw как выражение, которое не возвращается

Нельзя печатать `return throw ...` или подставлять statement `throw` в аргумент. На Java создать минимальный внутренний runtime helper `throw_value(Object value): Object`, приводящий value к `Throwable` и безусловно бросающий через существующий `sneaky_throw`. Его вызов работает в value и discard позициях, не создаёт функцию и сохраняет identity. Пример `(do (throw error) 42)` должен компилироваться, хотя `42` не исполняется: прямой `throw` перед сгенерированным `return` даёт javac `unreachable statement`. Ради удаления helper не добавлять анализ terminating paths.

JS value-position `throw` может использовать минимальную IIFE только для самой инструкции выбрасывания; discarded `throw` — native statement. Это узкое исключение для `throw`, не для `try`: аргумент сначала проходит обычный statement lowering, и вложенный `(throw (try ...))` не должен помещать `try` в новую IIFE. Аргумент вычисляется один раз внутри своей текущей защищённой области; порядок соседних операндов сохраняется.

Это технический выбор реализации, не дополнительное пользовательское API. Для Java `null` и неподходящего значения не вводить новую диагностику: действуют native NPE/ClassCastException или ошибка Java typing; гарантированный успешный операнд — `Throwable`. Форма со сложным RHS не должна выносить его за enclosing `try`.

### Неподдерживаемые targets

Eval добавляет точечные guards на достигнутые `try`/`throw` до вычисления содержимого; никаких новых `value` constructors. FEX добавляет формы в существующую проверку неподдерживаемых expressions, сохраняя цитирование. Это защита границы target, а не реализация обработки ошибок на этих targets.

### Проверки и документация

Поскольку eval не поддерживает API, новые execution samples размещаются в `test/samples/js/` и `test/samples/java/`, а не общем каталоге. Общие по смыслу сценарии используют одинаковые ожидаемые журналы; host-конструкторы различаются. Проверки значений используют `str` с бинарными `=`/`not=`, а identity исключений — host identity, не сравнение сообщений.

Покрыть успех, восстановление, все позиции выражений, ошибки host API и callbacks, rethrow, cleanup после ошибки обработчика, двойную ошибку, вложенное открытие/закрытие, прекращение чтения/финализации, сохранение registry и falsey результатов. Отдельно проверить Java `Error`, checked exceptions, typed callbacks/gen-class bridge, JS falsey thrown values и Promise boundary. Модели ресурсов могут вести журнал открытия/закрытия: внешний REPL не требуется.

Execution tests недостаточны для архитектурного требования: добавить небольшие generation/структурные OCaml-тесты на отсутствие wrappers для `try` в return, аргументе, условии, RHS `let`, discarded позиции, nested `if`/`do`, операнде `throw` и top-level `def`. Использовать простые источники без пользовательских lambdas и без самостоятельного JS value-`throw` для проверок отсутствия `Supplier`, `=>`/IIFE; случаи callbacks проверять отдельно, различая настоящую lambda и лишнюю обёртку. Не привязывать tests к точному номеру `Gensym` или форматированию всего файла.

Regression-матрица, которую требуется создать с нуля:
- Catch возвращает `(let [e "inner"] e)` при внешнем catch binding `e`; два соседних `let [x ...]` в handler; после вложенного shadowing сравнение/rethrow использует исходный catch binding; closure из handler сохраняет пойманный объект.
- `(try 1 2 (finally nil))` и `(try (throw error) (catch e 1 2))` возвращают `2`; discarded try с финальным host `void` вызовом компилируется; финальный try внутри `^void:java.lang.Runnable` тоже компилируется; эта metadata не ломает `(fn [] nil)` на JS.
- Журналы порядка `before/work/cleanup/after`, исключение раннего операнда, два `try` в одном вызове, dynamic callee и host receiver с эффектами, невыполненная ветка `if`, отложенный callback и порядок top-level definitions.
- Java top-level initializer с успешно пойманной checked exception; direct methods и typed callbacks с checked exception; scope/typing полученных temporaries и сохранение source maps.

Точечные OCaml-тесты покрывают grammar rejection, сохранение защищённых границ и unsupported targets; новый suite зарегистрировать в `test/dune`, если он нужен. Английский справочник описывает syntax, различия targets, приоритет ошибки очистки и host interop без обещания eval/FEX поддержки. Полная проверка — `make test`.

После реализации и проверок выполнить code review всего diff относительно этих specs и issue #38. Подтверждённые замечания автоматически исправлять в согласованном объёме, затем повторять затронутые проверки и `make test` после изменения кода/тестов; исправленный diff повторно ревьюить до отсутствия подтверждённых нерешённых замечаний. Предложения вне scope фиксировать отдельно, не внедрять молча.

## Risks / Trade-offs

- [Hoisting за границу `try`] → отдельная ветка общего lowering и execution tests с ошибочным RHS `let`/`if`/`do`.
- [Повторное появление wrapper через expression-do] → statement consumers на границах функций/definitions и generation tests на вложенные позиции, а не только execution tests.
- [Перестановка эффектов при hoisting] → материализация более ранних операндов и execution tests порядка/ошибок до входа в try.
- [Java return/throw, void, checked exceptions и типы temporaries] → helper только для throw, существующие bridges на нужных host-границах и реальная компиляция через `javac`, включая static initialization, value и void callbacks.
- [Утечка или коллизия catch binding] → fresh host identifier и scoped context; тест внешнего одноимённого binding и вложенного обработчика.
- [Ошибка cleanup скрывает исходную] → принятый native контракт, явное описание в справочнике и тест identity конечной ошибки.
- [JS Promise ошибочно воспринимается как защищённая async-операция] → документированная синхронная граница и тест без unhandled rejection.
- [REPL не исследован непосредственно] → тестируем модели сценариев issue; миграция потребителя отдельным change.

## Migration Plan

Под базовым кодом для выполнения понимается результат завершённого `preserve-if-statement-lowering-order`. Наблюдения об исходной ревизии выше служат историческим контекстом.

Выполнить полный `tasks.md` от базового кода; не полагаться на наличие файлов предыдущей попытки или её зелёные тесты. Обновить compiler, Java runtime с helper для throw и английский справочник вместе. Пользовательские вызовы с именами `try`/`throw` нужно переименовать. Перенос REPL и отдельный `with-open` выполняются позднее; этот change их не создаёт. Откат требует вернуть compiler/runtime согласованной парой и не использовать новые формы в исходниках. Коммит, завершающий issue, должен содержать `Closes #38`; коммит и архивирование — по отдельному запросу.
