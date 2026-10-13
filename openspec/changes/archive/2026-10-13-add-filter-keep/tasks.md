# Tasks

## 1. Filter и keep на трёх targets

- [x] 1.1 Добавить общие fixtures в `test/samples/` для отбора исходных сообщений и преобразования карточек, различий nil/false/0/пустой строки/текстового nil, пустого входа и полностью пустого результата, list/vector, неизменности входа, порядка и однократности callback, именованных и локальных функций во вложенном namespace. Проверка: fixtures выводят результаты count/get и бинарных сравнений через str с ожидаемой первой строкой и обнаруживают отсутствие новых bindings до реализации.
- [x] 1.2 Реализовать и зарегистрировать `filter`/`keep` в `backend_eval/eval_stdlib.ml` через существующие apply/truthy и List.filter/List.filter_map. Дополнить `test/eval_stdlib_contracts_test.ml` проверками согласованных ошибок аргументов, callback на пустой коллекции, арности при вызове и остановки с сохранением предыдущих эффектов. Проверка: новые eval fixtures и contract cases проходят.
- [x] 1.3 Добавить JS exports в `prelude/language_runtime.js` и имена в runtime import `backend_compiler/js.ml`; использовать language truthy для filter, исключать null/undefined в keep. Добавить JS fixture для прямого host undefined, false/0/пустой строки, identity объектов, неизменности плотного массива, unary callback и остановки при исключении по образцу run_callback_error. Проверка: общие и JS-specific fixtures проходят с обновлённым runtime.
- [x] 1.4 Добавить Java runtime методы в `prelude/language_runtime.java` через существующие call_fn/truthy и новый ArrayList. Добавить проверку остановки при исключении по образцу Java run_callback_error. Проверка: generated Java для общих fixtures, включая namespace callbacks, компилируется и возвращает ожидаемые результаты; Java-specific проверка проходит.
- [x] 1.5 Обновить английский `skills/y2k-language/SKILL.md`: API обеих функций, различие truthiness и nil, немедленный обход list/vector, JS undefined, ограничения входов и eval-диагностика. Проверка: примеры соответствуют sample-тестам и документированное поведение совпадает с delta specs; выполнить `make test` после изменений тестов и реализации.

## 2. Интеграционная проверка

- [x] 2.1 Подтвердить успешный итоговый `make test`, включающий сборку, копирование runtime в пакет и suites eval/JS/Java; использовать уже полученный результат 1.5, если после него код и тесты не менялись. Проверка: все suites проходят, compiler использует обновлённые runtime exports.
- [x] 2.2 Проверить согласованность реализации и всех сценариев delta specs; выполнить `openspec validate add-filter-keep --strict`. Проверка: строгая валидация успешна, каждый сценарий обеспечен соответствующей проверкой поведения.
