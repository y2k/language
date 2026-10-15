# Tasks

## 1. Композиция JS без изменения поведения

- [x] 1.1 До правки сохранить эталонный JS для существующих общих и JS samples и целевых примеров imports/exports, raw-code и interop; запускать каждый source отдельным процессом. Проверить наличие результатов и зафиксировать команду сравнения.
- [x] 1.2 Перевести внутреннюю сборку `backend_compiler/js.ml` на отложенные фрагменты с `text`/`concat`/`join` и финальной записью в `Buffer`, сохранив публичный строковый результат. Сравнить все результаты с эталонами побайтово, включая служебные имена и форматирование.
- [x] 1.3 Выполнить `make test`, форматирование изменённых OCaml-файлов, `git diff --check` и `openspec validate refactor-js-emission --strict`; записать фактические результаты приёмки. Пользовательская документация не требует изменений, поскольку поведение сохраняется.

## Проверки

- Эталоны: 90 непустых результатов (86 общих/JS samples и 4 целевых примера), каждый получен отдельным процессом до правки генератора.
- Скрипт и эталоны: `/private/var/folders/vy/q595y6l936gggt042s884ww00000gn/T/opencode/refactor-js-emission/` (`check.py`, `before/`).
- Команда сравнения после `dune build ./bin/main.exe`: `python3 /private/var/folders/vy/q595y6l936gggt042s884ww00000gn/T/opencode/refactor-js-emission/check.py after`.
- После рефакторинга: все 90 результатов совпали побайтово.
- `make test`: успешно, включая 283 sample/contract-теста и целевые suites eval, JS и Java.
- `ocamlformat --inplace backend_compiler/js.ml` и `ocamlformat --check backend_compiler/js.ml`: успешно; после форматирования повторное сравнение всех 90 результатов также успешно.
- `git diff --check` и `openspec validate refactor-js-emission --strict`: успешно.
- Независимое ревью итогового diff: замечаний нет.

## Workflow follow-up

- После приёмки рефакторинга можно отдельно начать `inline-js-source-maps`.
