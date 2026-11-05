# Design

## Context

Мотивация описана в `proposal.md`. `bin/main.ml` читает stdin, а `Runner.run` возвращает текст для eval/JS/Java. FEX сохраняет этот интерфейс: один source компилируется в один текстовый bundle. Материализация файлов принадлежит `add-flat-definition-extractor`, исполнение — `add-flat-definition-interpreter`; они не являются условием готовности compiler.

## Goals / Non-Goals

**Goals:** чистая компиляция в плоские выражения через текстовый runner, отдельный RHS на файл после распаковки, case-sensitive именованные bindings, простой контракт для будущего исполнителя.

**Non-Goals:** extractor/unpacker, FEX decoder, loader, evaluator, host adapters, reload, полный host interop, изменения внешнего пакета make, доставка, объединение bundles, benchmarks и stress tests.

## Decisions

### 1. Backend и CLI

Добавить `backend_compiler/fex.ml`: формировать именованные RHS в исходном порядке и сериализовать их в текстовый bundle для `Runner.run`. Использовать существующий parse/desugar под `Gensym.run` и общие helpers раскрытия форм. Не использовать JS/Java statement lowering: FEX непосредственно представляет `let`, `if`, `fn`. Пользовательские locals сохраняют имена, временные создаются через `Gensym`.

`backend_compiler/dune` не содержит явного списка modules: файл включается автоматически. Разделить генерацию именованных RHS/records и их текстовую упаковку настолько, насколько это нужно compiler fixtures: задачи 1.1–1.2 тестируются напрямую без CLI. Минимальное разрешение имён входит уже в генерацию; задача 2.2 дополняет покрытие canonical keys, а не откладывает необходимую для 1.1 recursion/shadowing реализацию.

`--target fex` читает source из stdin и выводит только bundle в stdout, без требования output directory и без записи code-файлов. `--source-map` остаётся только у JS. Отдельный режим `--unpack-fex --output-dir PATH` и его модуль определяются в `add-flat-definition-extractor`.

Сейчас `bin/main.ml` использует `print_endline` для любого результата. Для FEX нужен `print_string`: завершающие LF уже принадлежат bundle. Сохранить прежнюю печать остальных targets. Пустой source или один `ns` дают пустой bundle, а не пустую строку. Проверка `source_map && target <> "js"` уже есть в runner; добавить FEX regression test, а не вторую проверку. `--source-file` без карты остаётся не влияющим на output. Compiler failures сейчас могут быть `Failure`, не только `Error` runner; новая общая система ошибок не требуется.

Bundle содержит ровно две строки на определение: canonical key, затем стандартный Base64 с padding полного текста RHS, включая его переводы строк. Обе строки заканчиваются LF. Заголовка, trailer, счётчика и дополнительной пустой строки нет. Внешний Base64 не меняет внутренние записи FEX, включая Base64 строковых значений. Например, `(ns app.logic) (def answer 42)` даёт:

```text
app.logic.answer
aW50IDQyCg==
```

Контракт чтения контейнера, файловой записи и stdout unpacker находится в capability `flat-definition-extractor`. Compiler определяет только транспорт и содержимое RHS.

Один source → один bundle сохраняет модель правила `packages/make/0.6.0/make.clj` (`ly2k --target ... < source > output`). Этот пакет сейчас безусловно требует `language_runtime.<target>`; отключение этого требования для FEX относится к отдельному изменению make. Распаковка и доставка, например `adb push --sync`, выполняются после компиляции. Управление несколькими bundles остаётся shell, без отдельного API объединения. Обновление файлов не выполняет initializers и не запускает reload.

```sh
ly2k --target fex < app.clj > app.fex
ly2k --unpack-fex --output-dir out < app.fex > load-order.txt
```

Пары с Base64 выбраны вместо архива или заголовков со счётчиком строк ради простого writer/unpacker и существующего текстового интерфейса. Цена — увеличение размера и временный payload одного определения в памяти unpacker; runtime продолжает читать распакованный FEX построчно.

### 2. Исходные формы и имена

Source содержит необязательный `ns` и верхнеуровневые `def` после desugar; default namespace — `user`. Поддерживаются literals, quote, коллекции, вызовы, функции, условия, последовательности, bindings и деструктуризация. Alias разрешает имя без загрузки зависимости. Lexical bindings и собственные определения учитываются при затенении builtin; recursion представляется `global`.

В поддержанном source один необязательный `ns` находится в начале, как в текущем справочнике compiled targets. Frontend выдаёт `(compiler/ns "namespace" (("required" "alias") ...) (("Class" "package.Class") ...))`; использовать эти данные, не evaluator namespace loader и не JS/Java symbol munging. Учитывать имена всех top-level `def` данного source, включая более поздние: local имеет приоритет над собственным global, собственный global — над builtin. Qualified reference разрешает alias либо содержит полный namespace. Остальные свободные имена становятся globals текущего namespace; наличие значения проверяет будущий runtime, а не compiler. Неизвестное имя не становится новым builtin.

Существующие macros — `defn`, `def-`, `defn-`, `cond`, `case`, `and`, `or`, `if-let`, `->`, `->>`, keyword lookup, `let`, `fn`, vector/map syntax. Использовать их общий desugar, если результат принадлежит FEX subset. `when` в frontend отсутствует и не добавляется. Private metadata не вводит отдельного visibility-механизма в формате: RHS всё равно адресуется canonical key.

Frontend лишь нормализует patterns в `(list ...)`/`(hash-map ...)`; реальные `get` bindings создаёт `Lowering_expression_to_statement.lower_binding`. Переиспользовать этот helper для одного pattern/value и вложенных patterns, переводя возвращённые symbol bindings в вложенные FEX `let`. Не вызывать `lower`, `lower_value` или `lower_bindings`, которые выносят выражения в statements. Для non-symbol параметра создать один параметр через `Gensym`, затем тем же helper раскрыть pattern в теле. RHS остаётся единственным value child временного binding. Так сохраняются последовательная видимость и вложенное shadowing; ограничение существующего JS/Java lowering не переносится в FEX.

`fn` с одним выражением ссылается прямо на него (как `calculate` ниже), с несколькими — на `do`; `let` с несколькими bindings вкладывается последовательно. `if` без else получает `nil`. Явный пустой `do` сохраняется, а не заменяется lowering-ом. Шесть параметров проверяются на FEX records, без Java `FnN`: guest ABI реализуется в interpreter change.

Имена builtins: `+ - * = not= < <= > >= not str list hash-map get count assoc conj map reduce atom deref reset! swap!`. Здесь генерируются ссылки `builtin`, их реализация относится к interpreter change. Host interop (`raw-code`, `.`, `new`, `set!`, `cast`, host imports) отклоняется при компиляции формы, без отдельного AST-валидатора.

В эту границу входят также `instance?`, `compiler/gen-class` после `gen-class`, `export-default` и строковые host `:require`. `.method` и `Class.` уже раскрываются в `.` и `new`; типизированный параметр `fn` — в `cast`. Отклонять эти формы в соответствующих ветках генерации, включая вложенные формы; цитированные данные не являются interop-вызовом. Nested `def` не входит в expressions. Не добавлять неперечисленные runtime builtins (например, `/`, `println`, `get-in`) только потому, что они существуют на других targets.

`quote` следует JS/Java targets: atoms становятся строковыми данными, списки — вызовами `list`. `'nil` кодируется строкой; литерал `nil` — записью `nil`.

`Desugar.desugar` обходит также содержимое `quote`: FEX получает уже раскрытые данные, как JS/Java. Например, `'[1]` цитирует `(list 1)` и даёт список строк `list`, `1`; `'(cond true 1)` цитирует `(if true 1 nil)`. Не добавлять quote-aware frontend и не обещать Clojure-семантику. Строки приходят в `SAtom` с внешними кавычками и уже декодированным содержимым; снять только внешние кавычки, не декодировать escapes повторно. Неизвестные escapes, включая `\u0041`, остаются буквальными парами согласно parser.

Canonical key — компоненты namespace и имя `def`, разделённые точками. В каждом компоненте `[a-z0-9_-]` сохраняются, ASCII `A–Z` кодируются как `!a–!z`, остальные UTF-8 bytes — `%HH` с заглавными hex digits; точки namespace — разделители. Это адаптация [Go module cache escaping](https://github.com/golang/mod/blob/master/module/module.go), а не принятие ограничений Go на имена. Буквальный `!` кодируется `%21`. `a.b/c` → `a.b.c`, `a/b.c` → `a.b%2Ec`; `app/foo`, `app/Foo`, `app/FOO`, `app/!foo` → `app.foo`, `app.!foo`, `app.!f!o!o`, `app.%21foo`. Правило применяется и к namespace, и к имени определения, сохраняя различия регистра на регистронезависимой FS без сканирования коллизий. Bundle key, filename и `global` совпадают; unpacker и runtime не снимают escapes и не кодируют key повторно. Ограничения длины filename файловой системы сохраняются.

### 3. Схема FEX

UTF-8 без BOM, одна запись на строку, разделитель LF. Поля разделены одним ASCII space. Пустых записей нет. Корень — строка 1, все child references — существующие последующие строки. Заголовка и version record нет.

| Запись | Содержимое |
|---|---|
| `nil`, `true`, `false` | Типизированные литералы |
| `int decimal` | Signed int32 |
| `float decimal` | Конечное binary64; допускается exponent notation |
| `string base64` | UTF-8 → стандартный Base64 с padding; `-` для пустой строки |
| `local name` | Имя lexical binding |
| `global full-name` | Полное имя определения |
| `builtin name` | Имя переносимого примитива |
| `fn body-ref params...` | Тело и именованные параметры, включая нулевую арность |
| `call callee-ref args...` | Callee и аргументы слева направо, включая нулевую арность |
| `if condition-ref then-ref else-ref` | Условие и две отдельные ветви |
| `let name value-ref body-ref` | Один binding; несколько раскрывает compiler |
| `do refs...` | Последовательность, включая пустую |

Для `(ns app.logic) (defn calculate [x] (+ x 1))` файл `app.logic.calculate`:

```text
fn 2 x
call 3 4 5
builtin +
local x
int 1
```

Внутри FEX Base64 применяется только к строковым значениям; внешний Base64 bundle кодирует весь RHS отдельно. Locals и параметры — symbol tokens без разделителей полей. По возможности выделить Base64 encoder из `backend_compiler/source_map.ml` в общий helper для строк и bundle, сохранив source-map поведение.

Минимальный вариант — непосредственно переиспользовать уже доступный `Source_map.base64`, который работает с bytes и выдаёт стандартный padding без LF. Выделение helper не является обязательной отдельной задачей. Parser не типизирует числа: FEX emitter должен различать int32 и finite binary64 и выдавать decimal payload, а не считать, что frontend уже проверил тип/диапазон. Проверить поддержанные границы int32, отрицательное целое, дробь и exponent; не вводить отдельный проход числовой валидации. Структура строится в порядке root → children слева направо; адреса назначаются в пределах одного RHS, начиная с 1.

Формат прямых выражений выбран ради простоты будущего исполнителя: TAB/custom escapes и JSON требуют более сложного decoder, стековый bytecode добавляет машинную семантику. Compiler может выполнять больше работы ради простого формата.

### 4. Проверка без интерпретатора

Целевые compiler/CLI fixtures проверяют точный bundle, декодированный RHS, адреса, раскрытие форм, canonical keys и оба слоя Base64 без зависимости от extractor. Файловые проверки принадлежат extractor change. Вложенные closures, recursion, lazy `if` и порядок эффектов здесь проверяются по структуре записей, а исполнение этих сценариев принадлежит interpreter change. Не создавать тестовый evaluator ради проверки compiler.

Добавить целевой Alcotest executable `test/fex_test.ml` в `test/dune` с самого первого теста. Общий sample-runner `test/test.ml` запускает eval/JS/Java и для JS дописывает `(print-result (test))`: его нельзя использовать как FEX runner без отдельного изменения. FEX fixtures должны содержать только допустимые top-level формы и не должны требовать запуска guest-кода. CLI-проверки использовать по образцу `test/js_source_map_test.ml`: process status/stdout/stderr; сравнивать bytes без `String.trim`. Запуск в пустом временном каталоге проверяет отсутствие code-файлов. Base64 проверять известными payloads и независимым decoder (например, Node `Buffer`, уже используемым тестами source maps), не только round-trip собственным encoder.

`Gensym.run` устанавливает handler, но глобальный counter не сбрасывается. Для fixtures с temporaries проверять связи имён и fresh bindings, не фиксировать номера `G__N` и не обещать побайтовую стабильность повторных компиляций в одном процессе. Точный golden `calculate` не содержит temporaries. `make test` требует `LY2K_PACKAGES_DIR`, выполняет `dune build`, копирует существующие JS/Java runtimes и запускает `dune runtest --profile test`; регистрация в `test/dune` включает FEX suite без нового Makefile target.

### 5. Сверка предпосылок с репозиторием

Проверено 2026-11-05 на `cfa1372` (до реализации FEX). Таблица отделяет существующие механизмы от работы этого change; при изменении этих файлов перед apply сверить соответствующую строку заново.

| Требование / задачи | Проверенный источник | Следствие для реализации |
|---|---|---|
| Общий frontend, 1.1 | `frontend/frontend.ml`, `builtin_macros.ml`; `test/frontend_desugar_test.ml` | Parse/desugar готов; `cond` существует, `when` отсутствует. Новые macros не нужны. |
| Patterns, 1.1 | `builtin_macros.ml:normalize_binding_pattern`, `lowering_expression_to_statement.ml:lower_binding`; `test/lowering_test.ml` | Нормализация frontend не завершает destructuring; переиспользовать binding helper, сохранив FEX scope. |
| Имена, 1.1/2.2 | `frontend/macro_ns.ml`, `backend_compiler/symbol_munge.ml` | Namespace AST готов; FEX resolver и canonical encoding новые, JS munging не подходит. |
| Quote, 1.1/1.3 | `frontend/desugar.ml`, `js.ml:compile_quote`, `java.ml:compile_quote` | Сохранить post-desugar quote, не менять frontend. |
| Literals/strings, 1.1/1.2 | `frontend/parser.ml`; `test/frontend_desugar_test.ml:string_escapes` | Типизация чисел новая в emitter; строки уже декодированы. |
| Records/addresses, 1.1/1.2 | Существующего FEX backend нет; `lowering_expression_to_statement.ml` не подходит целиком | Все tags и numbering реализуются здесь, без evaluator. |
| Temporaries, 1.1/1.2 | `frontend/gensym.ml` | Использовать handler; не считать counter локальным компиляции. |
| Bundle/CLI, 2.1 | `bin/runner.ml`, `bin/main.ml`; `test/js_source_map_test.ml` | Новый dispatch и FEX-only печать без добавочного LF. |
| Base64, 1.2/2.1 | `backend_compiler/source_map.ml:base64` | Существующий encoder пригоден для обоих слоёв. |
| Interop/errors, 2.3 | `builtin_macros.ml:fn_macro/method_call_macro/constructor_macro/gen_class_macro`, `macro_ns.ml`, `bin/runner.ml` | Проверять формы после desugar; source-map guard уже общий. |
| Документация, 1.3/2.4 | `skills/y2k-language/SKILL.md`, `references/js.md`, `references/java.md` | Добавить `references/fex.md` и ссылку/CLI-раздел; явно отличить FEX subset от общей библиотеки. |
| Test wiring, 1.1/2.4/3.1 | `backend_compiler/dune`, `test/dune`, `test/test.ml`, `Makefile` | Модуль подключается автоматически, test executable регистрируется явно; FEX не четвёртый executable sample target. |
| Make compatibility, 2.4 | `/Users/igor/project/packages/make/0.6.0/make.clj:makefile` | Правило stdin/stdout совместимо, весь пакет пока нет: требует `language_runtime.fex`. |
| Межchange-контракт, 3.2 | Design/specs extractor и interpreter | Bundle — только transport; decoder получает распакованный RHS; keys, tags, Base64 и порядок совпадают. |

При сверке исполнены JS compiler probes для `cond`, `'nil`, `'[1]`, `'(cond true 1)`, параметра `^String`, вложенной деструктуризации и строковых escapes; результаты соответствуют описанию выше. Python standard Base64 подтверждает `int 42\n` → `aW50IDQyCg==`. Это проверка предпосылок плана, не доказательство готовности будущего FEX backend.

## Risks / Trade-offs

- Структурные проверки не доказывают runtime-семантику → сквозные проверки вынесены в зависимый interpreter change.
- Подмножество уже полного языка → явно перечислить формы и builtins в справочнике.
- Формат связывает compiler с будущим decoder → хранить контракт здесь, не дублировать независимые версии схемы в runtime change.
- Потеря читабельности и рост bundle из-за внешнего Base64 → для просмотра использовать распакованные RHS; не усложнять runtime транспортным decoder.

## Migration Plan

Добавить `fex` как opt-in текстовый target, обновить английский справочник `skills/y2k-language/` примером source → bundle. Подключить compiler/CLI-тесты к `make test`, сохранив существующие eval/JS/Java suites. Изменения make и доставка оформляются отдельно. Готовность этого change не требует extractor или исполнения FEX на hosts. Далее реализуются `add-flat-definition-extractor` и `add-flat-definition-interpreter`.
