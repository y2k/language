# Tasks

## 1. Исходная форма и общий контракт компиляции

- [x] 1.1 Добавить проверки в `test/frontend_desugar_test.ml` и сохранить raw-code с исходными операндами в `frontend/desugar.ml`; проверить, что литерал, keyword и выражение не преобразуются внутри формы, а остальные макросы работают как прежде.
- [x] 1.2 Добавить `backend_compiler/raw_code.ml` с извлечением payload и локальными проверками арности/литерала. По согласованию удалить обход позиций и его вызов в lowering, а также тесты гарантированной ранней диагностики.
- [x] 1.3 Сохранить корректные raw-code в lowering как непрозрачные инструкции без временного результата; проверить последовательность нескольких вставок, функции/лямбды, `let`/`do` и условный путь AST-тестами без дополнительных bindings для payload.
- [x] 1.4 Описать поддержанные statement-only позиции и reserved-name совместимость на английском в `skills/y2k-language/SKILL.md`; указать отсутствие гарантированной диагностики неподдержанных позиций.

## 2. Генерация JavaScript

- [x] 2.1 Создать `test/raw_code_test.ml`, зарегистрировать suite в `test/dune` и реализовать JS top-level/statement dispatch в `backend_compiler/js.ml` с локальным отказом expression-emitter; проверить точный payload и границы вставок, отсутствие вызова raw_code, обёрток, return и добавленного `;`, пустую строку, quote и ошибки аргументов при генерации.
- [x] 2.2 Добавить в JS suite проверки multiline, пробелов, кавычек, обратного слеша, LF/TAB/CR, неизвестного escape и line comment на конце; подтвердить побайтовое сохранение декодированного текста без повторного decoding и отделение следующей инструкции переводом строки.
- [x] 2.3 Добавить target-specific fixtures в `test/samples/js/` для module declaration, function/lambda, `let`/`do`, порядка эффектов и выбранной/пропущенной ветки; проверить выполнение через sample-runner и добавить проверяемые JS-примеры/ограничения в английский справочник. По согласованию исправить генерацию пустого lowered let* statement-блока в JS; проверить общей регрессией `discarded_if_blocks.clj` без raw-code.

## 3. Генерация Java

- [x] 3.1 Реализовать Java raw-code в `compile_discard` и top-level `compile_statement` с локальным отказом expression-emitter; расширить `test/raw_code_test.ml` для точного payload, пустой строки, escapes/комментариев, quote и ошибок аргументов при генерации. Проверить отсутствие дополнительного `;`, return или static initializer.
- [x] 3.2 Добавить target-specific fixtures в `test/samples/java/` для class member, function/lambda, `let`/`do`, порядка эффектов и условной вставки; проверить generated Java через `javac` и исполнение sample-runner. Описать в английском справочнике class placement, host identifiers и отсутствие Java imports вне класса; сверить примеры с fixture.

## 4. Отказ evaluator

- [x] 4.1 Добавить в `backend_eval/eval.ml` явное отклонение достигнутого raw-code перед вычислением операндов; покрыть в `test/raw_code_test.ml` корректный литерал, неверную арность, аргумент с reset!, цитирование и недостигнутую ветку. Проверить Eval_error с поддержкой только JS/Java, сохранение counter и error result runner.
- [x] 4.2 Добавить в английский справочник описание eval rejection без вычисления аргументов; проверить соответствие описания и примеров тестам evaluator.

## 5. Интеграционная проверка

- [x] 5.1 Запустить `make test` и подтвердить прохождение сборки, целевых suites и samples всех трёх targets, включая регрессии обычных строк, interop и lowering.
- [x] 5.2 Запустить `openspec validate add-raw-code --strict` и проверить итоговый diff: реализация, проверки и справочник соответствуют контракту, runtime-файлы и зависимости не изменены.

## Результаты проверки

После согласованного упрощения повторный `make test` прошёл: raw-code suite — 7 проверок, lowering — 3, samples — 232. Строгая валидация change и `git diff --check` прошли; изменённые OCaml-файлы отформатированы. Проверки исходных позиций и отказа для свёрнутого `(do "text")` удалены, тесты валидной генерации сохранены.

`make test` и принудительный запуск `dune runtest --profile test --force` прошли; sample suite содержит 232 проверки. `openspec validate add-raw-code --strict`, `git diff --check` и `ocamlformat --check` для изменённых OCaml-файлов прошли. Независимое read-only ревью не нашло Critical/Important проблем.

Отложенное Minor замечание: усилить JS scope-регрессию behavioral-проверкой host var из условного блока вместо узкого сравнения текста IIFE. Текущая реализация использует обычный statement block; интеграционные регрессии порядка эффектов и условных путей проходят. Замечание reviewer об устаревших checkboxes устранено обновлением этого файла.
