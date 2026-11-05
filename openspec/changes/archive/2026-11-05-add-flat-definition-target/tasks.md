# Tasks

## 1. Compiler и формат

- [x] 1.1 Добавить `backend_compiler/fex.ml` (Dune включает модуль автоматически) и сразу зарегистрировать `test/fex_test.ml` в `test/dune`. Через существующий parse/desugar реализовать literals, `defn`/`cond` и остальные поддержанные раскрытия, post-desugar quote, коллекции, функции и последовательные bindings. Patterns раскрывать через `Lowering_expression_to_statement.lower_binding`, не вызывая statement lowering всего дерева; non-symbol параметры заменять через `Gensym`. Проверить records для вложенной деструктуризации, однократного RHS, shadowing, recursion, ссылок на более поздний `def`, шести параметров и захвата locals. Реализация resolver нужна уже здесь; task 2.2 расширяет её fixtures. Не добавлять frontend macros и не фиксировать номера `G__N` в golden tests.
- [x] 1.2 Реализовать space/Base64 serialization с переиспользованием `Source_map.base64` и 1-based forward references внутри каждого RHS. Проверить точный `calculate`, все tags, порядок callee/аргументов, обе ветви `if` и `nil` при отсутствии else, тела из нескольких выражений, вложенные `let`, пустой `do`, нулевую арность fn/call, int32 boundaries и конечные float/exponent payloads. Проверить пустую строку, Unicode и parser-decoded escapes (включая сохранённый неизвестный escape) известными Base64 payloads/независимым decoder; не исполнять FEX.
- [x] 1.3 Добавить английский `skills/y2k-language/references/fex.md`: формат, таблицу кодов, post-desugar quote (включая `'nil` и `'[1]`), список builtins и границы форм. Сверить пример с generated RHS, выполнить `make test` после изменения тестов.

## 2. Текстовый bundle

- [x] 2.1 Подключить `--target fex` к текстовому runner/CLI и help. Bundle — пары canonical key / Base64 полного RHS без дополнительных строк, заголовка и trailer; для FEX использовать печать без дополнительного LF, оставив прежнюю печать eval/JS/Java. Сравнить bytes Runner и CLI без trim: точный `answer`, несколько и повторные определения в исходном порядке, пустой source и source только с `ns`. Проверить отсутствие code-файлов в пустом временном каталоге, игнорирование `--source-file` без карты и сохранение существующих интерфейсов.
- [x] 2.2 Завершить проверки единого кодирования filename/bundle/global keys и resolver: default `user`, symbolic alias из `compiler/ns` без загрузки зависимости, полная qualified ссылка, local/own-global/builtin приоритет. Проверить `a.b/c` против `a/b.c`, `foo`/`Foo`/`FOO`, буквальные `!` и `%`, регистр namespace и Unicode; проверить совпадение bundle key с `global` для тех же имён. Не использовать `Symbol_munge` и не добавлять проверку case-only коллизий каталога.
- [x] 2.3 Проверить согласованные ошибки compiler: существующий runner guard `--source-map` для FEX; `raw-code`, `.`, `new`, `set!`, `cast`, `instance?`, `gen-class`, `export-default`, host imports/строковые requires, включая `.method`, `Class.` и cast из типизированного параметра после desugar. Проверить запрет top-level выражений кроме namespace/def и nested `def`; цитированные interop-формы остаются данными. Диагностировать в генерации без отдельного AST validator и без рефакторинга общей системы ошибок. Выполнить `make test` после изменения тестов.
- [x] 2.4 Обновить `skills/y2k-language/SKILL.md` (таблицу targets и CLI-раздел) и `references/fex.md`; выполнить пример через собранный `dune exec ./bin/main.exe -- --target fex`, эквивалентный документированному `ly2k`. Описать внешний Base64, canonical keys, FEX subset и модель «один source → один bundle». Явно указать, что make-пакет пока требует отсутствующий `language_runtime.fex`, а unpacker/interpreter — отдельные changes; не выдавать pipeline из будущих режимов за доступный сейчас. Убедиться, что compiler/CLI fixtures уже подключены к `make test` через `test/dune`, и выполнить его.

## 3. Итоговая интеграция

- [x] 3.1 Выполнить `make test` для существующих eval/JS/Java suites и новых compiler/CLI-проверок без зависимости от FEX evaluator и без benchmark/timing/stress probes.
- [x] 3.2 Сверить generated format со spec `compiler-targets`, выполнить `openspec validate add-flat-definition-target --strict` и зафиксировать фактические команды проверки.

## Проверка реализации 2026-11-05

- `make test` после исправления review: 17 suites, 512 tests успешно (включая 10 FEX tests). Regression для переполнения host integer сначала воспроизвёл ошибочное преобразование в float, затем прошёл; добавлен fixture вложенного shadowing с последующим обращением к внешнему binding.
- `openspec validate add-flat-definition-target --strict` — valid.
- `git diff --check` и `ocamlformat --check backend_compiler/fex.ml test/fex_test.ml bin/main.ml bin/runner.ml` — успешно.
- Пример `(ns app.logic) (def answer 42)` выполнен через `dune exec ./bin/main.exe -- --target fex`: stdout точно `app.logic.answer\naW50IDQyCg==\n`.
- Generated format сверен со spec через точные records/bytes fixtures: tags, forward addresses, quote, обе ветви if, canonical keys, Base64 строки и bundle; независимый Node decoder проверяет внешний payload. Review подтвердил границы compiler-only реализации; оба замечания устранены.

## Порядок и проверка предпосылок

До реализации сверить таблицу «Сверка предпосылок с репозиторием» в `design.md` с текущими исходниками, особенно если они изменились после указанной там ревизии. Не считать упоминание формы в документации доказательством её наличия: проверить frontend/compiler helper и существующий тест или выполнить минимальный probe. Новое поведение FEX отличать от переиспользуемого поведения языка. Обнаруженное расхождение исправлять в proposal/design/spec/tasks согласованно до продолжения реализации.

1.1 создаёт records и resolver, 1.2 — их сериализацию, 2.1 — bundle и CLI. Тестовый executable подключается в 1.1, не в конце работы. Задача 2.2 проверяет всю матрицу имён на готовом bundle. FEX fixtures не добавляются как исполняемые сценарии общего sample-runner: он работает с eval/JS/Java и дописывает host entry points. После новых тестов запускать `make test`; уже успешный полный прогон можно использовать для итоговой 3.1, если после него не менялись code/tests.

Проверки плана 2026-11-05 не закрывают implementation tasks. На исходном `cfa1372` выполнен `make test`: 16 suites, 502 tests, успешно. Дополнительно выполнены probes текущего JS compiler для предпосылок, перечисленных в design; FEX backend на момент проверки отсутствует.

`openspec validate --all --strict`: все три FEX changes проходят; пять существующих main specs (`application-skill`, `compiler-targets`, `eval-runtime`, `fractional-arithmetic`, `frontend-syntax`) не проходят strict из-за предупреждений о длине requirement text (>500 символов). Эти main specs данной правкой не изменяются. Проверять готовность этого плана отдельной командой `openspec validate add-flat-definition-target --strict`; предупреждения main specs не являются доказательством дефекта или готовности FEX implementation.
