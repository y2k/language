## Why

Парсер сохраняет source-последовательность `\n` как два символа, поэтому `eval`, JavaScript и Java передают пользователю видимый текст `\n` вместо перевода строки. Это нарушает conventional семантику строк и уже ломает интеграции, отправляющие многострочный текст.

## What Changes

- Декодировать `\n` внутри quoted source string в один LF до выполнения или target-specific escaping.
- Сохранить одинаковое runtime-значение строки для `eval`, JavaScript и Java.
- Добавить parser-проверку, отличающую LF от двух символов `\` и `n`, и cross-backend regression fixture.
- Сузить открытый вопрос о string escapes до остальных, пока не определённых escape-последовательностей.
- **BREAKING**: source `\n`, который ранее создавал literal backslash и `n`, начнёт создавать LF.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `frontend-syntax`: quoted source string декодирует `\n` в один LF.
- `open-questions`: неопределённость string escape semantics больше не включает `\n`, но сохраняется для остальных escape-последовательностей.

## Impact

- `frontend/parser.ml`: декодирование `\n` на общей границе разбора source.
- `test/frontend_desugar_test.ml`: точная проверка parsed string payload.
- `test/samples/`: общий runtime fixture для `eval`, JavaScript и Java.
- `backend_eval/` и `backend_compiler/` получают декодированный LF через существующий AST; изменения backend-кода и зависимостей не требуются.
