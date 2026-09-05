## Context

See `proposal.md` for motivation and `specs/compiler-targets/spec.md` for required behavior. Сейчас map literal раскрывается в `(hash-map ...)`, а `backend_compiler/js.ml` компилирует единственный аргумент `export-default` обычным `compile_expr`. JavaScript runtime создаёт `hash_map` через `Object.create(null)`, что несовместимо с проверкой Cloudflare Module Workers.

До backend все keywords уже раскрываются в строковые atoms. JavaScript generator может использовать эту существующую canonical representation без target-specific изменений общего frontend; исходный string literal намеренно принимается как эквивалентный key.

## Goals / Non-Goals

**Goals:**

- Проверить непустую последовательность пар непосредственно в JavaScript backend.
- Создать обычный object без промежуточного language hash map.
- Сохранить однократное вычисление values слева направо средствами JavaScript object literal.

**Non-Goals:**

- Менять представление или поведение `hash_map`.
- Поддерживать старый `(export-default expression)` или автоматически мигрировать source.
- Добавлять Cloudflare/Wrangler dependency либо выполнять Cloudflare validation локально.
- Добавлять общую JavaScript object interop form.

## Decisions

### Разбирать пары только в JavaScript generator

JavaScript generator распознает desugared `(export-default "key" value ...)`, проверит непустой список полных пар и выдаст `export-default: ...` для malformed forms. Общий frontend продолжит только существующее раскрытие keywords, поэтому `:fetch` и исходный `"fetch"` принимаются как эквивалентные keys.

Это удерживает target-specific форму в `backend_compiler/js.ml` и не требует нового macro или внутреннего `compiler/export-default`. Строго различать keyword и string literal после desugaring невозможно, но такое различие не влияет на generated object и не оправдывает расширение общего frontend.

### Генерировать object literal с computed string properties

JavaScript generator выдаст один статический `export default`:

```js
export default {["fetch"]: handler, ["scheduled"]: scheduled_handler};
```

Computed properties сохраняют ключ `"__proto__"` как own data property вместо специального prototype setter object literal. Новый объект поэтому имеет обычную цепочку через `Object.prototype`, а values компилируются существующим `compile_expr`.

Альтернативы отклонены:

- Изменение `hash_map` на `{}` затронуло бы все language maps.
- Object spread всё равно сначала создал бы null-prototype map и не нужен при новом pair syntax.
- Новый runtime helper добавил бы API и import без необходимости.

### Использовать существующие тестовые уровни

Существующий `default_export` в `test/js_ns_test.ml` будет переведён на новый syntax и проверит точный object-literal output с несколькими handlers; соседний test проверит malformed forms. Отдельная Cloudflare integration dependency не нужна.

## Risks / Trade-offs

- [Старые исходники перестанут компилироваться] -> Миграция явно заменяет map argument на пары `keyword value`.
- [String literal также принимается как key] -> После общего desugaring он идентичен keyword и создаёт то же observable property; строгая frontend validation не нужна.
- [Duplicate keys не диагностируются] -> Используется обычная семантика JavaScript object literal: последняя пара побеждает; отдельная проверка не требуется.
- [Exact string test чувствителен к форматированию] -> Это существующий стиль `test/js_ns_test.ml` и минимальная проверка требуемой генерации.

## Migration Plan

1. Заменить `(export-default {:fetch handler})` на `(export-default :fetch handler)`; дополнительные handlers записывать следующими парами.
2. Для rollback откатить изменение JavaScript generator и вернуть старый source syntax; persisted data и runtime migration отсутствуют.
