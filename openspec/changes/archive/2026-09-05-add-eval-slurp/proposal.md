## Why

Программы, исполняемые target `eval`, не могут прочитать текстовый файл без добавления специальной логики в host-приложение. Простая stdlib-функция `slurp` позволит получить всё содержимое файла как строковое runtime-значение.

## What Changes

- Добавить в eval stdlib одноаргументную функцию `(slurp path)`.
- Читать указанный текстовый файл целиком и возвращать его содержимое без преобразования.
- Разрешать relative path от текущего рабочего каталога процесса.
- Возвращать контролируемый `Eval_error` при неверном числе или типе аргументов и при ошибке чтения файла.
- Не добавлять `slurp` в targets `js` и `java`.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `eval-runtime`: stdlib target `eval` получает функцию `slurp` и её success/error contract.

## Impact

- `backend_eval/eval_stdlib.ml`: реализация и регистрация `slurp`.
- `test/samples/eval/`: три eval-only integration samples по аналогии с target-specific директорией `test/samples/java/` — успешное чтение, неверные аргументы и ошибка чтения.
- `test/test.ml`: eval sample path сравнивает ожидаемый `Runner.Error` с первой строкой error sample, не меняя запуск `js` и `java` samples.
- Публичное поведение изменяется только для target `eval`; новые зависимости не требуются.
