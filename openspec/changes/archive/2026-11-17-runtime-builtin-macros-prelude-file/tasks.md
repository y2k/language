# Tasks

## 1. Файл prelude в каталоге пакетов

- [x] 1.1 Сверить `${LY2K_PACKAGES_DIR}/prelude/1.0.0/macro/prelude.clj` с контрактом design (решение 2): top-level определения `cond`, `and`, `or`, `with-open-bindings`, `with-open` в этом порядке, тела совпадают с `macros/builtin/*.clj` после замены имён `expand-*` на имена макросов; файл уже добавлен в репозиторий пакетов коммитом `ea9c12d`; при расхождении исправить его там отдельным коммитом и обновить SHA для задачи 3.1. Проверка: сравнение с `macros/builtin/*.clj` после замены имён не показывает различий, кроме пробелов.

## 2. Загрузка определений из файла

- [x] 2.1 Перевести `Language_macros.create_from` на список имён форм и исходник (разбор только базовыми макросами, поиск точек входа по именам форм), а стандартное расширение — на ленивое чтение `${LY2K_PACKAGES_DIR}/prelude/1.0.0/macro/prelude.clj`; удалить генерацию `Builtin_macro_source` из `macros/dune` и файлы `macros/builtin/*.clj`; в `test/dune` объявить зависимость от `LY2K_PACKAGES_DIR` и этого файла. Адаптировать существующие тесты (`probe` под своим именем, `choose` через `(def choose cond)`, ожидания `eval_deps_test`) и добавить проверку, что рекурсивный вызов по имени формы внутри исходника не раскрывается как макрос. Проверка: `dune build` проходит, `rg "Builtin_macro_source|expand-" macros test` ничего не находит, `make test` проходит.
- [x] 2.2 Реализовать fail-fast: `LY2K_PACKAGES_DIR` не задана или пуста, файл отсутствует или не читается, ошибка разбора или вычисления, нет определения одной из четырёх форм. Добавить тесты: программа без этих макросов работает при незаданной `LY2K_PACKAGES_DIR`; использование `cond` при отсутствующем файле даёт ошибку с ожидаемым путём; файл без определения `with-open` даёт ошибку при первом раскрытии. Проверка: новые тесты проходят в `make test`.
- [x] 2.3 Обновить `skills/y2k-language/SKILL.md` (описание `cond`/`and`/`or`/`with-open` как «bundled with the compiler») и раздел Build and Test в `README.md`: макросы загружаются из `${LY2K_PACKAGES_DIR}/prelude/1.0.0/macro/prelude.clj`, программы с ними и `make test` требуют этот файл. Проверка: `rg "bundled with the compiler" skills README.md` ничего не находит, описание совпадает с delta-спекой.

## 3. Docker

- [x] 3.1 В `Dockerfile` скопировать `macros/` и `fex_unpack/`, добавить `ARG PACKAGES_REF` с полным SHA коммита репозитория пакетов, заранее создать `/tmp/language-packages/prelude/1.0.0/macro` от имени `opam` и загрузить туда `prelude.clj` через `ADD --chown=opam:opam https://raw.githubusercontent.com/y2k/packages/${PACKAGES_REF}/prelude/1.0.0/macro/prelude.clj`; в разделе Docker `README.md` описать запуск программ с макросами через том каталога пакетов и `-e LY2K_PACKAGES_DIR`. Проверка: `docker build --platform linux/amd64 -t language .` проходит вместе с `make test`; `printf '(cond false 1 true 42)\n' | docker run --rm -i --platform linux/amd64 language` завершается ненулевым кодом и упоминает `LY2K_PACKAGES_DIR`, а с подключённым каталогом пакетов печатает `42`.

## 4. Интеграционная проверка

- [x] 4.1 Выполнить `make test` с `LY2K_PACKAGES_DIR`, указывающей на каталог пакетов, и убедиться, что сборка и suites `eval`, `js`, `java` проходят.
- [x] 4.2 Выполнить `openspec validate runtime-builtin-macros-prelude-file --strict` и убедиться, что валидация проходит без ошибок и предупреждений.

## Workflow follow-up

- Пушить в `main` после выполнения всех задач, включая Docker-сборку.
- После реализации и ревью заархивировать change и синхронизировать delta-спеки с `openspec/specs/`.
