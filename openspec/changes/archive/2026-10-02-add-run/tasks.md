# Tasks

## 1. Функция run! и её контракт на трёх targets

- [x] 1.0 Исправить согласованные namespace-препятствия: Java method references используют текущий helper class, JS sample-runner запускает файл по namespace-пути рядом с runtime; добавить общий fixture с named callback map и проверить его на eval, JS и Java до и после исправлений.
- [x] 1.1 Добавить `test/samples/run_effects.clj` с namespace `checks.effects.core`, проверяющий named/lambda/local callback, порядок и однократность вызовов, немедленное завершение, пустой обход и настоящий ложный nil через бинарные `=`/`not=` и `str`; до реализации убедиться, что fixture обнаруживает отсутствие run!.
- [x] 1.2 Реализовать binding run! в `backend_eval/eval_stdlib.ml` через `List.iter` и существующий apply; добавить проверки в `test/eval_stdlib_contracts_test.ml` для runtime-конструктора Nil, согласованных арностей/типов, не-callable на пустом list, callback arity при вызове и остановки на `assertion failed` с сохранением эффектов; проверить eval suite и общий fixture на eval.
- [x] 1.3 Реализовать `run_BANG_` в `prelude/language_runtime.js` и добавить runtime import в `backend_compiler/js.ml`; проверить JS-выполнение общего fixture, включая вложенный namespace и равенство результата null, а также целевой host-catch тест остановки обхода при ошибке callback.
- [x] 1.4 Реализовать `run_BANG_` в `prelude/language_runtime.java` с существующим call_fn и возвратом null; проверить компиляцию и Java-выполнение общего fixture с named callback, а также целевой host-catch тест остановки обхода при ошибке callback.
- [x] 1.5 Добавить `test/samples/js/run_dom_array.clj` с host-массивом DOM-подобных объектов и callback из #26; проверить исходные объекты/selector, порядок querySelector/click, однократность кликов и nil существующим JS sample-runner без новых зависимостей.
- [x] 1.6 Обновить `skills/y2k-language/SKILL.md`: API run!, list/vector, эффекты/ошибки, eval-проверки и Array.from для NodeList; сверить описание с обоими delta-specs и реализованными тестами, не обещая map/NodeList/nil-collection или единую compiled диагностику.

## 2. Интеграционная проверка

- [x] 2.1 Выполнить `make test` с окружением проекта и подтвердить сборку, обновление package runtime copies и прохождение suites eval, JS и Java, включая новые fixtures.
- [x] 2.2 Выполнить `openspec validate add-run --strict` и проверить diff: только согласованные изменения, без новых форм AST/lowering, зависимостей или изменений поведения map/reduce; подтвердить отсутствие collection accumulation в реализациях run!.
