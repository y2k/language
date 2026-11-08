# Tasks

Полный план реализации с нуля. Базовая ревизия и самостоятельный запуск описаны в `proposal.md`; архитектура — в `design.md`, обязательное поведение — во всех трёх delta specs. Все задачи намеренно незавершены: текущий рабочий diff, прежние fixtures и прежние результаты тестов не требуются и не подтверждают новый дизайн. Каждый указанный новый файл нужно создать при отсутствии. Отмечать задачу только после её проверки; не восстанавливать старые галочки автоматически.

Предварительное условие: завершить `preserve-if-statement-lowering-order`. Общий рефакторинг if и порядка операндов вынесен туда; задачи ниже добавляют exceptions и проверяют их интеграцию с готовым lowering. После отката пробной реализации все задачи снова не выполнены.

## 1. Grammar и общий statement lowering

- [x] 1.1 Добавить общий parser секций try (например, `backend_compiler/exception_forms.ml`) и распознавание try/throw до generic-list traversal. Проверять непустые тела, хотя бы одну секцию, порядок/число catch/finally, простой catch symbol и арность throw; quote сохранять данными. Добавить допустимые и согласованные ошибочные случаи в `test/lowering_test.ml`; проверить targeted suite.
- [x] 1.2 Реализовать value-try как `(statements, result)` с fresh temporary и присваиваниями результата внутри body/handler; discarded try и cleanup lower-ить только в statements. Переиспользовать `lower_body_value`, `lower_body_discard`, `statement_block`, `Gensym` и готовый путь if/do из предварительного change. Структурные tests должны подтверждать result assignment, отсутствие результата в discard и отсутствие пользовательских вычислений вне защищённых секций.
- [x] 1.3 Сохранить lexical scope вложенных let и catch при lowering: реальные statement blocks, локальное shadowing без утечек, предыдущие формы в discard-контексте. Добавить structural tests для handler shadowing и нескольких соседних let; проверить suite и сохранение экспортируемого lower_binding для FEX.
- [x] 1.4 Проверить интеграцию try с передачей statement prefixes из `preserve-if-statement-lowering-order`: сохранение ранних значений до позднего prefix, синтаксических имён interop, metadata/типов. Добавить try-specific targeted tests порядка dynamic callee, receiver, аргументов, collection values и невыполняемой ветки; проверить lowering suite. Реальное исполнение этих cases — обязательная часть групп 2 и 3; общий рефакторинг if/операндов здесь не повторяется.
- [x] 1.5 Обновить `skills/y2k-language/SKILL.md` полным синтаксисом, JS/Java-only границей, результатами nil/false, scope, native cleanup и throw. Сверить текст с frontend/compiler delta specs; проверить `make test`, включая существующие программы без новых форм.

## 2. JavaScript: statements до места исполнения

- [x] 2.1 Генерировать native try/catch/finally как statements в `backend_compiler/js.ml`; научить body/definition consumers принимать lowered prefixes и итоговое значение без try-specific IIFE. Проверить структурно generated code для return, аргумента, условия, RHS let, discarded позиции, nested if/do и top-level def: нет обёртки непосредственно или через enclosing expression-do.
- [x] 2.2 Реализовать однократное вычисление throw operand с native discard throw и минимальной value-throw IIFE при необходимости; вложенный try в operand заранее lower-ить в statements. Создать `test/samples/js/` с успехом, recovery nil/false, rethrow identity, host ошибкой, callback, ошибкой handler/cleanup, falsey thrown values и Promise boundary без unhandled rejection. Проверить Node execution через `make test`.
- [x] 2.3 Создать JS samples порядка effects: ранний operand, try и cleanup, поздний operand, вызов; ранняя ошибка до try; два try в одном вызове; dynamic callee/receiver; коллекции; невыполненная ветка; deferred callback; top-level initialization. Добавить scope regressions из design, включая JS lambda с Java void metadata. Проверить результаты/журналы через `make test`.
- [x] 2.4 Добавить targeted source-map test вызова внутри lowered try и обновить `skills/y2k-language/references/js.md`: syntax, host message/logging, cleanup precedence и synchronous Promise boundary. Проверить исходную позицию ошибки, примеры справочника и `make test`.

## 3. Java: native try, typing и host boundaries

- [x] 3.1 Генерировать native try с catch(Exception ...) и statement cleanup в `backend_compiler/java.ml`. Доставлять prefixes в методы, реальные lambdas и существующие static initializer blocks; не создавать Supplier/callback/helper-метод для try, включая nested if/do/let. Добавить generation tests для всех value/discard позиций и def initializer; проверить отсутствие лишних wrappers и реальную компиляцию javac.
- [x] 3.2 Добавить scoped разрешение catch binding через fresh identifier и минимальный общий binding helper; сохранить ссылки в closures, внутреннее shadowing и native Exception type. Statement blocks должны иметь Java braces. Создать Java samples для handler `(let [e "inner"] e)`, non-final shadow/rethrow, двух соседних let с одним именем, outer scope/finally и closure capture; проверить JVM execution.
- [x] 3.3 Обработать value/void callback bodies в правильном контексте, учесть checked exceptions на границах static initialization, typed callbacks и gen-class существующим sneaky_throw. Создать tests non-final literals в try/catch, discarded try с финальным void call, финального try в Runnable, checked host exception и Java Error с cleanup без catch. Проверить javac/JVM и отсутствие изменения JS поведения Java metadata.
- [x] 3.4 Добавить `throw_value(Object): Object` в `prelude/language_runtime.java` поверх существующего sneaky_throw с cast к Throwable; генератор вызывает helper без lambda. Создать samples single evaluation вложенного let/do/if/try, rethrow identity, throw в аргументе и `(do (throw error) 42)` без javac unreachable-statement ошибки. Проверить отсутствие исполнения после throw и совпадение объектов.
- [x] 3.5 Создать Java samples порядка effects из группы 2, включая раннюю ошибку, dynamic callee/typed host receiver, коллекции, два try в вызове и top-level definitions. Проверить typing materialized values, порядок initialization и успешную обработку checked exception в initializer через javac/JVM и `make test`.
- [x] 3.6 Обновить `skills/y2k-language/references/java.md`: Exception против Throwable/Error, .getMessage/host logging, void позиции, finally precedence и совместное обновление compiler/runtime. Проверить примеры целевыми tests и запустить `make test`.

## 4. Границы eval и FEX

- [x] 4.1 Добавить в `backend_eval/eval.ml` явный отказ для достигнутых try/throw до исполнения содержимого; проверить отсутствие побочных эффектов, корректные недостигнутые формы и quote в OCaml-тестах по образцу `test/raw_code_test.ml`. Обновить `skills/y2k-language/references/eval.md`; проверить `make test`.
- [x] 4.2 Добавить try/throw в существующий rejection path `backend_compiler/fex.ml`; расширить `test/fex_test.ml` отказом в executable forms, включая неисполняемую ветку, и сохранением цитированных данных. Обновить `skills/y2k-language/references/fex.md`; проверить FEX CLI/runner tests и `make test`.

## 5. Сценарии issue #38

- [x] 5.1 Создать парные целевые samples JS/Java, моделирующие reload: только одна ветвь уведомления, сохранение прежнего registry при ошибке, очистка loader state, сохранение уже выполненных эффектов и продолжение последующих событий. Проверить результаты и журналы через `make test`.
- [x] 5.2 Создать целевые сценарии ресурсов с EOF, ошибками чтения/декодирования/финализации, неудачным открытием второго ресурса и ошибкой внутреннего закрытия; проверить закрытие открытых ресурсов, внешнюю очистку и отсутствие успешной финализации после ошибки. Описать вложенный try/finally в справочнике без with-open; проверить `make test`.
- [x] 5.3 Добавить helper ожидаемой ошибки в fixtures: нормальные nil/false дают отрицательный результат, исключение — положительный и не прерывает следующие проверки. Проверить бинарное равенство/истинность, а не только печатный текст; запустить `make test`.

## 6. Интеграция, review и автоматические исправления

- [x] 6.1 Выполнить `make test`, `openspec validate add-exception-handling --strict` и `git diff --check`; зафиксировать фактические результаты. Отдельно проверить generated source без Supplier/IIFE/helper вокруг try во всех контекстах из design: зелёные execution tests без структурных проверок недостаточны.
- [x] 6.2 Провести code review полного implementation diff относительно всех specs и design: statement lowering, порядок effects, отсутствие wrappers, catch/let scope, Java typing и reachability, static initialization, callbacks, identity и однократность cleanup. Результат — конкретные замечания с файлами/строками либо явный вывод об их отсутствии.
- [x] 6.3 Проверить замечания по коду и воспроизведениям; автоматически исправить подтверждённые проблемы в scope, добавить regression tests и выполнить затронутые проверки. Для каждого замечания зафиксировать исправление или обоснованный итог разбора; предложения вне scope перечислить отдельно.
- [x] 6.4 После исправлений повторить `make test` при изменениях кода/тестов и review исправленного diff до отсутствия подтверждённых нерешённых замечаний. Финально проверить strict OpenSpec validation и `git diff --check`; сообщить фактические результаты и ограничения.

## Workflow follow-up

Итог 6.4: после полного независимого review исправленный lowering diff повторно проверен основным агентом: fresh catch применяется на JS/Java, ссылки разрешаются в существующем lexical env, внешний scope и правила прочих bindings сохранены. Подтверждённых нерешённых замечаний нет. Послеправочный `make test` — зелёный (311 sample cases, 53 source-map, 12 lowering и остальные suites); strict OpenSpec validation, `git diff --check`, `ocamlformat --check` прошли. Проверялись Node/javac/JVM и модели issue #38; внешние REPL/Android-проекты не входят в change.

Исправление 6.3: regression `test/samples/js/exception_catch_names.clj` воспроизвёл SyntaxError на `default` (RED). Catch теперь использует `bind ~fresh:true` на обоих targets, сохраняя scoped resolution существующего lowering. Regression с `default`, `arguments`, `truthy` прошёл (GREEN); полный `make test` после исправления прошёл: 311 sample cases, 53 source-map tests, 12 lowering tests и остальные suites. Все замечания review устранены; расширений scope нет.

Review 6.2: независимый reviewer прочитал весь implementation diff, новые файлы и все planning artifacts. Найдено одно P2: `backend_compiler/lowering_expression_to_statement.ml:132` создаёт fresh catch binding только для Java; JS catch с `default`/`arguments` даёт SyntaxError, а `truthy` затеняет runtime helper. Других подтверждённых замечаний нет. За scope остаются согласованные async/Promise, native Java Error/cast/initialization правила, suppressed exceptions/rollback/with-open, общая гигиена существующих let/параметров и внешняя REPL/Android-интеграция.

Проверка 6.1: `make test` прошёл (310 sample cases, 53 source-map tests, 12 lowering tests и остальные suites). Structural tests проверяют native try в value/discard/initializer-контекстах и try в операнде throw, без дополнительных wrappers. `openspec validate add-exception-handling --strict`, `git diff --check` и форматирование изменённых OCaml-файлов прошли.

- В новой сессии выполнить `/opsx-apply add-exception-handling`, читая весь каталог change. Незакоммиченные implementation-файлы предыдущей попытки не являются зависимостью.
- `with-open`, миграция REPL и реальные Android-device проверки — отдельные задачи.
- При коммите, завершающем issue, добавить `Closes #38`. Коммит и архивирование — по отдельному запросу пользователя.
