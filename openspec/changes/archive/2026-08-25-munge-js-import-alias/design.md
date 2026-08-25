## Context

`macro_ns` передаёт алиасы `:require` в compiler как строковые атомы. JavaScript compiler извлекает алиас для string require и сразу вставляет его в ESM import; остальные JavaScript identifiers уже обрабатываются через `Symbol_munge.munge`.

## Goals / Non-Goals

**Goals:**

- Использовать одинаковое munging-поведение для алиаса в import и в обращениях к required namespace.
- Сохранить module specifier строкового `:require` без изменений.

**Non-Goals:**

- Не изменять правила munging и не добавлять общую валидацию JavaScript identifiers.
- Не изменять Java target или обработку символьных namespace requires.

## Decisions

- Применять существующий `Symbol_munge.munge` только к локальному алиасу при генерации string-require import. Это переиспользует единые правила компилятора и приводит import к уже существующей форме обращения `async-hooks/member` -> `async_hooks.member`.
- Не обрабатывать module specifier: `"node:async_hooks"` является внешним ESM-путём, а не identifier, поэтому должен сохраниться буквально.
- Добавить один тест в `test/js_ns_test.ml` с алиасом `async-hooks` и точным ожидаемым import. Существующие строковые imports остаются покрыты без изменения поведения.

## Risks / Trade-offs

- Алиасы с другими синтаксическими ограничениями JavaScript, которые не решает текущее `Symbol_munge.munge`, остаются вне области изменения → это согласуется с обработкой прочих identifiers и может быть рассмотрено отдельно при появлении требования.
- Проверка generated source не запускает импортируемый Node module → тест фиксирует контракт генератора и не вводит внешнюю runtime-зависимость.
