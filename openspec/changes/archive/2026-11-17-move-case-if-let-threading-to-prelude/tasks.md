# Tasks

## 1. Prelude в каталоге пакетов

- [x] 1.1 Добавить в `${LY2K_PACKAGES_DIR}/prelude/1.0.0/macro/prelude.clj` после `with-open` определения `case`, `if-let`, `->`, `->>` и их helpers по design (решения 3–4) и закоммитить их в репозитории пакетов без push. Проверка: `make test` на текущем `main` языка с обновлённым файлом проходит — дополнение совместимо с прежним компилятором.

## 2. Перенос в языковое расширение

- [x] 2.1 Добавить native `syntax-list?`, расширить `prelude_names` до `cond`, `and`, `or`, `with-open`, `case`, `if-let`, `->`, `->>`, удалить OCaml-обработчики `case`, `if-let`, `->`, `->>` из `macros/builtin_macros.ml`. Адаптировать `test/pipeline_test.ml`: заменить `->` и `case` в роли OCaml-макросов на оставшиеся builtin-формы и проверить, что восемь форм не раскрываются при пустом списке расширений и во frontend-only разборе. Проверка: `rg "case_macro|if_let|thread_macro" macros` ничего не находит, `make test` проходит, включая `frontend_desugar_test` и `js_source_map_test`.
- [x] 2.2 Добавить тесты для новых сценариев delta-спек: отклонение `(if-let [a/b value] ok)` сообщением `if-let: binding names must be symbols`; metadata списка пары `if-let`; `(-> x (f a) g)`, `(->> x (f a) g)` и шаг-вектор `(-> x [f a])`; файл prelude только с четырьмя прежними определениями даёт ошибку про `case` при раскрытии `cond`; вложенные `deps` с `case`, `if-let`, `->`, `->>`. Проверка: новые тесты проходят в `make test`.
- [x] 2.3 Обновить `skills/y2k-language/SKILL.md`: список макросов из файла prelude, строку `if-let` (только простые неквалифицированные имена) и строку `->` (шаг, не являющийся списком, вызывается со значением); обновить раздел Build and Test в `README.md`: перечень макросов из файла prelude и необходимость обновлять каталог пакетов вместе с компилятором. Проверка: каждый перечень макросов из файла prelude в `rg -n "with-open" skills/y2k-language/SKILL.md README.md` включает `case`, `if-let`, `->` и `->>`, описание совпадает с delta-спеками.

## 3. Docker

- [x] 3.1 После подтверждения пользователя опубликовать коммит из 1.1 в `y2k/packages`, обновить `ARG PACKAGES_REF` в `Dockerfile` на его полный SHA и при необходимости раздел Docker в `README.md`. Проверка: `docker build --platform linux/amd64 -t language .` проходит вместе с `make test`; `printf '(case 1 1 42 0)\n' | docker run --rm -i --platform linux/amd64 language` без тома завершается ненулевым кодом и называет `LY2K_PACKAGES_DIR`, а с подключённым каталогом пакетов печатает `42`.

## 4. Интеграционная проверка

- [x] 4.1 Выполнить `make test` с `LY2K_PACKAGES_DIR`, указывающей на обновлённый каталог пакетов, и убедиться, что сборка и suites `eval`, `js`, `java` проходят.
- [x] 4.2 Выполнить `openspec validate move-case-if-let-threading-to-prelude --strict` и убедиться, что валидация проходит; единственное допустимое сообщение — INFO о порядке архивации до архивации `runtime-builtin-macros-prelude-file`.

## Workflow follow-up

- Заархивировать `runtime-builtin-macros-prelude-file` раньше этого change: delta `internal-language-macros` изменяет добавленное им требование.
- Пушить в `main` после выполнения всех задач, включая Docker-сборку.
- После реализации и ревью заархивировать change и синхронизировать delta-спеки с `openspec/specs/`.
