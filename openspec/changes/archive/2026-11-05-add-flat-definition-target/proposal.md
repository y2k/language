# Proposal

## Why

Нужен переносимый результат компиляции y2k с отдельным RHS для каждого определения, сохраняющий существующий подход compiler: source из stdin, текстовый результат в stdout. Этот change задаёт FEX, compiler и bundle; материализация файлов вынесена в `add-flat-definition-extractor`, исполнение — в `add-flat-definition-interpreter`.

## What Changes

- Добавить текстовый target `fex`: пары строк «canonical key / Base64 полного RHS» в исходном порядке, без заголовка и trailer.
- Поддержать case-sensitive имена на регистронезависимых файловых системах: ASCII `A–Z` кодируются как `!a–!z` по подходу Go module cache; filename, bundle key и `global` используют один canonical key.
- Определить плоский формат выражений с адресами строк, именованными переменными, разделителем ASCII space и UTF-8/Base64 для строковых значений.
- Проверять структуру и точное содержимое сгенерированных файлов без интерпретатора.
- Документировать CLI, формат и поддерживаемое подмножество языка.
- Переиспользовать существующие макросы (`defn`, `cond` и другие формы, раскрывающиеся в поддержанные core-формы), не добавляя новые формы языка. Деструктуризацию завершать при генерации FEX с переиспользованием существующего binding helper, без statement lowering всего дерева.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `compiler-targets`: текстовый target `fex`, контракт bundle, canonical keys и FEX payload.

## Impact

Затрагиваются новый `backend_compiler/fex.ml`, `bin/main.ml`, `bin/runner.ml`, регистрация compiler/CLI-тестов в `test/dune` и английский справочник `skills/y2k-language/`. `backend_compiler/dune` автоматически включает новый модуль; отдельного списка modules там нет. Доступны `Lowering_expression_to_statement.lower_binding` и `Source_map.base64`; новый frontend macro или runtime не требуется. Runner сохраняет текстовый результат для всех targets; CLI печатает FEX bundle без дополнительного LF, сохраняя прежний `print_endline` для eval/JS/Java. Модель «один source → один bundle» совместима с правилом компиляции `packages/make/0.6.0/make.clj`, но пакет пока требует отсутствующий FEX runtime; его поддержка FEX оформляется отдельно. Unpacker принадлежит `add-flat-definition-extractor`, loader/evaluator и reload — `add-flat-definition-interpreter`. Доставка, объединение bundles и performance/stress-проверки не входят в scope.
