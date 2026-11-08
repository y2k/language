# Tasks

## 1. Безопасные prefixes

- [x] 1.1 Исправить сохранение ранних операндов и JS lookup callee/метода/конструктора до prefixes поздних аргументов; сохранить this без ранней проверки callable. В Java различать direct named/static Class/method и local/dynamic call, не распознавать класс в receiver-позиции `.`. Добавить failing regressions в lowering tests и JS host samples, исправить общий lowering и JS emitter, проверить single evaluation, getters, mutable host values, collections, cast/instance? через lowering suite и make test.

## 2. Согласованный переход if и consumers

- [x] 2.1 Заменить scoped consumers обычными statement blocks с внешним result temporary для value-let; Java result может быть Object, тип задаётся существующим внешним binding/parameter hint либо cast. Сохранить lower_if `(statements, result)`, предыдущую область initializer и исходное окружение поздних аргументов, включая host-имена. Не переименовывать user locals ради переноса consumer; проверить raw-code local reads и выходящий Java return. Сохранить discarded/void, callbacks, static checked-call bridge и lower_binding для FEX. Добавить regressions scope/raw-code и structural tests отсутствия IIFE/Supplier/helper/switch; выполнить targeted suites и make test, сохранив if_value.clj `7|12|3|nil|false|3|ab`.
- [x] 2.2 Дополнить общие `test/samples/` проверками порядка before/condition/selected/after, двух if в одном вызове, dynamic callee, коллекций, shadowing, невыполненной ветки, deferred callback и последовательных def. Проверить shadowing initializer и сохранение внешнего binding в позднем аргументе с if после раннего операнда с одноимённым локальным binding. Проверять значения nil/false через бинарное равенство, а не только печать; выполнить `make test` на eval/JS/Java.
- [x] 2.3 Обновить javac/JVM regressions receiver/callable из let на явные внешние hints/casts, включая new/host method, `.concat` → `"ab"` и поздний аргумент с if. Проверить сохранение ранней host-ошибки, Java void callback и checked initializer, JS source maps условия/ветки. Обновить английский Java-справочник о границе Object и поддержанных местах type hints, не вводя expression hint. Выполнить Node, javac/JVM и make test.

## 3. Интеграция

- [x] 3.1 Выполнить review полного diff на порядок эффектов, scope, typing и отсутствие wrappers; исправить подтверждённые проблемы с regression tests. Проверить `make test`, `openspec validate preserve-if-statement-lowering-order --strict`, `git diff --check` и записать фактические результаты.

Итоговые проверки: `make test` — все suites прошли, 296 sample cases и 52 source-map tests; strict OpenSpec validation, `git diff --check` и ocamlformat check изменённых OCaml-файлов прошли. Финальное review обнаружило захват Reflect; regression воспроизведён и исправлен доступом к intrinsic apply через literal. Повторное review: за апрув.

## Workflow follow-up

- [x] 3.2 Не материализовать литерал nil перед поздними prefixes; проверить Java-перегрузки и эффекты do/if в `test/samples/java/nil_operand_overload.clj`, выполнить make test и strict validation. Regression RED → GREEN; все suites прошли, 297 sample cases и 52 source-map tests; strict validation и git diff --check прошли.

- После завершения этого change продолжить `add-exception-handling` поверх проверенного lowering. Не реализовывать try/throw в этом change.
- Коммит и архивирование — по отдельному запросу.
