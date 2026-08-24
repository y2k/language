## Context

`frontend/macro_ns.ml` сохраняет происхождение require: символьный namespace становится обычной строкой во внутренней форме `compiler/ns`, а исходный строковый module specifier сохраняет вложенные кавычки. `backend_compiler/js.ml` сейчас одинаково преобразует оба случая в относительный путь. См. `proposal.md` и delta spec `specs/compiler-targets/spec.md`.

## Goals / Non-Goals

**Goals:**

- Генерировать валидные bare ESM imports для строковых `:require`.
- Сохранить текущий путь генерации локальных символьных namespace requires.
- Защитить оба пути compiler tests.

**Non-Goals:**

- Не менять форму `ns`, macro expansion, evaluator или Java compiler.
- Не добавлять module resolution, проверку существования npm-пакетов или поддержку новых форм require.

## Decisions

### Использовать сохранённый признак строкового require в JavaScript generator

После снятия внешнего слоя строки из namespace значения строковый source specifier остаётся строковым literal, в отличие от символьного namespace. Генератор распознаёт этот признак: literal вставляется как есть в `from`, а namespace преобразуется в существующий относительный путь.

Это минимально ограничивает изменение JavaScript target, где возникает дефект, и сохраняет текущий внутренний контракт `compiler/ns` для evaluator и Java compiler.

Альтернатива: изменить `macro_ns` для явного тега source token type и обновить всех потребителей `compiler/ns`. Это устраняет неявное представление, но расширяет изменение на не затронутые issue backends без дополнительного наблюдаемого выигрыша.

### Проверять generated imports на уровне compiler test

Тесты в `test/js_ns_test.ml` уже сравнивают точный generated JavaScript. Отдельный пример со строковым module specifier вместе с существующим символьным примером проверит оба требуемых пути без запуска Node или установки пакетов.

Альтернатива: интеграционный тест через `node --test`. Он не нужен для проверки text generation и добавит зависимость от внешнего runtime.

## Risks / Trade-offs

- [Внутренний маркер происхождения строки неочевиден] → Документировать его в локальной логике generator и покрыть оба варианта точными тестами.
- [Строковые относительные specifier также останутся без преобразования] → Это соответствует контракту: любой строковый require является ESM module specifier, который должен быть сохранён.

## Migration Plan

Изменение не требует миграции данных или развёртывания. После compiler test можно сразу выпускать; rollback состоит в откате изменения generator и test.
