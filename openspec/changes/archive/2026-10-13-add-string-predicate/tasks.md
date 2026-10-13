# Tasks

## 1. Предикат и его контракт на трёх targets

- [x] 1.1 Добавить string? и регистрацию builtin в `backend_eval/eval_stdlib.ml` по образцу vector?; расширить `test/eval_stdlib_contracts_test.ml` проверками Bool true/false и Eval_error для нулевой/лишней арности. Проверка: новая строка матрицы проходит в eval suite.
- [x] 1.2 Добавить `string_QMARK_` в JS/Java runtime и JS runtime import. Добавить общий `test/samples/string_predicate.clj` для строк, keywords, всех перечисленных нестроковых категорий, значений из get, boolean-результата и однократного вычисления аргумента; проверить также вызов во вложенном namespace. Проверка: samples дают ожидаемый вывод на eval, JS и Java.
- [x] 1.3 Добавить целевые samples для quote в `test/samples/eval/`, `js/`, `java/`; в JS проверить JSON.parse, boxed String, undefined и обычный объект, в Java — String и StringBuilder. Проверка: целевые результаты совпадают с соответствующими delta specs, а не навязывают общую семантику quote.
- [x] 1.4 Обновить английский `skills/y2k-language/SKILL.md`: API string?, keywords, различия quote, host-типы и eval-арность. Проверка: каждый описанный случай соответствует specs и добавленным тестам.

## 2. Интеграционная проверка

- [x] 2.1 Запустить `make test` с настроенным `LY2K_PACKAGES_DIR`; проверить успешную сборку и suites всех трёх targets с обновлённым runtime.
- [x] 2.2 Выполнить `openspec validate add-string-predicate --strict` и проверить итоговый diff на соответствие согласованному контракту; критерий — успешная валидация и отсутствие несвязанных изменений.
