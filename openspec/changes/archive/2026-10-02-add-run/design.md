# Design

## Context

Мотивация описана в `proposal.md`. `map` уже реализован отдельно в eval и двух runtimes; eval использует общий `apply`, Java — `call_fn`, JS — обычный вызов функции. List/vector имеют общий runtime-тип: `List` в eval, массив в JS, `java.util.List` в Java. JS compiler импортирует фиксированный перечень runtime bindings, Java использует static wildcard import. Источники runtimes находятся в `prelude/`; `make build` копирует их в package directory.

Дизайн нужен для согласования изменений нескольких targets; отдельной подсистемы не требуется. Новые требования дополняют существующие specs, не меняя поведение `map`/`reduce` или их диагностику.

## Goals / Non-Goals

**Goals:**
- Встроить функцию в существующий путь вызова и подключения stdlib.
- Не выделять результирующую коллекцию и не вводить новые зависимости.
- Проверять переносимый сценарий существующим sample-runner.

**Non-Goals:**
- Новые формы AST, макросы, lowering, общие адаптеры коллекций и рефакторинг `map`/`reduce`.
- Расширенная переносимая диагностика неправильных программ на compiled targets.
- Браузерная инфраструктура тестов, поддержка sparse arrays, изменение состава коллекции во время обхода, асинхронное ожидание callback.

## Decisions

### Обычная runtime-функция

В `backend_eval/eval_stdlib.ml` добавить реализацию и binding `run!`; в `prelude/language_runtime.{js,java}` — `run_BANG_`; добавить имя в runtime import `backend_compiler/js.ml`. Existing symbol munging преобразует `!` в `_BANG_`, Java wildcard import не требует изменения подключения runtime.

При первом запуске namespace-fixture выявлены два существующих ограничения; их минимальные исправления согласованы отдельно. Java `compile_atom` жёстко указывает `user::` для top-level function values: передавать фактическое имя helper class в ctx и использовать его в method reference. JS sample-runner исполняет source через stdin в корне: сохранять module в соответствующем namespace-пути временного output root рядом с runtime и запускать Node по этому файлу. Покрыть оба исправления отдельным общим fixture с именованным callback `map`, не зависящим от нового run!.

Альтернатива — макрос поверх reduce — добавляет раскрытие и фиктивный аккумулятор, хотя требуемый обход уже выражается простым циклом. Map с отбрасыванием результата нарушает требование не собирать результаты.

### Прямой обход с существующим вызовом callback

- Eval: проверить шаблоном ровно два аргумента, `Closure` и `List`, выполнить `List.iter` с `ignore (apply fn [item])`, вернуть `Nil`.
- JS: проверить массив по существующему образцу `map`, пройти последовательным циклом, вызвать `fn(item)` и вернуть `null`. Не передавать callback непосредственно в `.forEach`, чтобы не добавлять индекс и массив к аргументам.
- Java: проверить `java.util.List` по образцу `map`, пройти enhanced for loop, вызвать существующий `call_fn(fn, item)`, вернуть `null` из метода с return type `Object` и `throws Exception`.

Нет перехвата ошибок callback или накопления его результатов. Проверка callback до пустого обхода обязательна для eval; новые межцелевые гарантии диагностики не вводятся. Прямой cast Java callback к `Fn1` не нужен: `call_fn` уже обслуживает вызовы языка.

### Узкая модель коллекции

Следовать `map`, а не `reduce_items`: только list/vector и обычные JS-массивы. NodeList пользователь преобразует через Array.from до вызова. Это избегает преобразования map entries, неопределённого переносимого порядка map и новых adapters.

### Проверки и документация

- Добавить `test/samples/run_effects.clj`: Atom-журнал и счётчик для порядка, однократности и eager-завершения; named callback и lambda/local callback; пустой обход; бинарные `=`/`not=` и проверка истинности результата. Выводить результаты через `str`, ожидания — в первой строке. Не сравнивать сами коллекции: JS equality использует identity.
- В `test/eval_stdlib_contracts_test.ml` проверить `Nil` непосредственно, согласованные ошибки аргументов, callback arity только при вызове и сохранение `assertion failed` с остановкой обхода и сохранением эффектов.
- Добавить `test/samples/js/run_dom_array.clj`: trusted top-level raw-code объявляет плотный host-массив объектов с querySelector/click и журналом; именованный callback языка использует interop из #26. Проверить selector, identity/порядок, clicks и nil. Это проверка интеграции с DOM-подобными объектами, не настоящий browser DOM.
- Для compiled ошибок callback добавить минимальные целевые проверки с доступным host catch, подтверждающие остановку и сохранение эффектов без фиксации host exception type.
- Обновить таблицу API и eval-контракт в `skills/y2k-language/SKILL.md`, описать ограничение list/vector и Array.from для NodeList.
- Полная проверка — `make test` с окружением проекта; runtime исходники не править в package copies.

## Risks / Trade-offs

- JS `undefined` ложен, но не равен `nil/null` через `_EQ_` --> явно вернуть `null`, проверять равенство и истинность, не только текст.
- Передача top-level функции в Java должна сохранить существующее представление FnN --> использовать имя текущего helper class вместо hardcoded user и покрыть named callback общим sample, не вводить новый механизм ссылок на функции.
- DOM-подобный fixture не доказывает работу в браузере --> он проверяет массив host-объектов и сгенерированный interop; реальный DOM остаётся на стороне userscript.
- Runtime import без обновлённой package copy ломает JS-модули --> выпускать compiler и runtime вместе через существующий `make build`.

## Migration Plan

Миграция программ и данных не нужна: API аддитивный. Обновить исходники runtimes и compiler, выполнить `make test` (включает build и копирование runtimes), затем использовать `(run! hide-thread! threads)` в пользовательском проекте отдельно от этого change. Откат — совместный возврат изменения compiler import и runtime/stdlb bindings; существующие программы без run! не затрагиваются.
