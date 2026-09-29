# Tasks

## 1. Boolean-модель и семантика

- [x] 1.1 Добавить `Bool of bool` и общий helper истинности, обновить literal lookup и quote; проверить в `test/eval_ns_test.ml` конструкторы boolean, сохранение Nil и текстовые `"false"`/`"true"`, включая quoted list.
- [x] 1.2 Перевести `if`, `not`, `assert`, `=`, `not=`, `vector?` и операции порядка на Bool; добавить общие samples истинности строк и управляющих макросов, обновить старые ожидания `nil_truthiness`; проверить, что все producers возвращают Bool, а условие и выбранная ветка вычисляются однократно.
- [x] 1.3 Добавить eval-only сценарии раздельных boolean/text map keys, let/fn-деструктуризации, функций и Atom; проверить сохранение ложного Bool и отсутствие равенства с текстом через бинарные проверки в `str`.
- [x] 1.4 Обновить в `skills/y2k-language/SKILL.md` истинность, not, равенство и quoted boolean; проверить, что справочник больше не объявляет `"false"` ложной в eval и сохраняет различия quoted values JS/Java.

## 2. Вывод и диагностика

- [x] 2.1 Обновить `to_string`, `value_text` и `bin/runner.ml` для Bool, адаптировать тестовые helpers по типу результата; проверить прямые финальные boolean, str коллекций, диагностику `(false)` и прежний пустой вывод коллекций/функций/Atom в runner-тестах.
- [x] 2.2 Уточнить раздел eval CLI справочника и добавить соответствующие runner regression cases; проверить, что вывод `true`/`false` не требует ручного `str`, а `(str false)` является истинным текстом.

## 3. Интеграционная проверка

- [x] 3.1 Выполнить `make test` после изменений tests; проверить сборку и suites eval, JS и Java без изменений поведения compiled targets.
- [x] 3.2 Перед синхронизацией specs согласовать baseline завершённых `add-eval-nil`/`add-atoms` и этого delta: учесть переименование runner в nil-change, сохранить сценарии nil, Atom и get-in; проверить согласованный delta командой `openspec validate add-eval-booleans --strict`. Старые deltas не применять поверх boolean.

## Проверки реализации

- Рабочее место: текущий checkout на main по явному выбору пользователя.
- RED: `make test` до реализации выявил три ошибки eval ns и четыре новых eval fixtures; общие boolean fixtures на JS/Java уже проходили.
- GREEN: `make test` после реализации прошёл; eval ns — 23 проверки, samples — 207 проверок, остальные suites без ошибок.
- Модель и scalar-вывод внесены совместно: новый конструктор требует обработки во всех exhaustive matches до успешной сборки.
- Справочник: независимая проверка до правки воспроизвела устаревшие ответы о ложности текста и quote; после правки все восемь вопросов о boolean/тексте/CLI получили ответы, согласующиеся с реализацией и тестами. Замечание об истинности в AGENTS.md также актуализировано.
- Baseline для будущей синхронизации зафиксирован в Migration Plan design.md; `openspec validate add-eval-booleans --strict` прошёл. Старые deltas и main specs не изменялись.
- Независимое read-only review реализации, fixtures, справочника и reconciliation record: замечаний нет. `git diff --check HEAD` прошёл. Отложенные scalar/contracts изменения, обобщение map-key equality, равенство функций/Atom и изменение shadowing остаются за пределами этого change согласно его Non-Goals.
- `ocamlformat --check` для всех пяти изменённых `.ml` файлов прошёл. Общий `dune build @fmt` дополнительно обнаружил прежнее форматирование `bin/dune` и `test/dune`; эти файлы не изменены (`git diff HEAD -- bin/dune test/dune` пуст). Автоформатирование посторонних файлов не выполнялось.
