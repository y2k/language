# Tasks

## 1. Контейнерный дистрибутив

- [x] 1.1 Добавить multi-stage `Dockerfile`: `builder` на `ocaml/opam:debian-12-ocaml-5.3`, Dune/Angstrom/Re/Alcotest, Node.js 24 и JDK 25; обеспечить права записи в рабочий каталог. Добавить `.dockerignore`. Проверка: Docker-сборка выполняет `make test` из исходников без локального `_build`.
- [x] 1.2 Добавить этап `runtime` на `debian:bookworm-slim`, скопировать только бинарник в `/usr/local/bin/language` и задать exec-form ENTRYPOINT. Проверка: `docker build --platform linux/amd64 -t y2khub/language:latest .` успешен; `ldd` не содержит неразрешённых зависимостей; в образе нет `ocaml`, `opam`, `dune`, каталога `/opt/language/packages` и предустановленного `LY2K_PACKAGES_DIR`.
- [x] 1.3 Проверить интерфейс контейнера: default target и `--target eval` для `(str "Hello, " "world!")` дают `Hello, world!` с переводом строки и кодом 0; `--target js` и `--target java` дают тот же stdout, что CLI того же checkout; `--target unknown` даёт ненулевой код и ожидаемый stderr.
- [x] 1.4 Добавить в README команды локальной сборки, pull/run со stdin и выбора target; описать `linux/amd64`, использование через `FROM` и ограничения `COPY --from`, внешние Node.js/JDK и runtime-файлы из репозитория для исполнения generated code. Проверка: документированные локальные команды запуска и выбора target работают с собранным образом.

## 2. GitHub Actions и публикация

- [x] 2.1 Создать `.github/workflows/docker.yml`: push только в `main`, `ubuntu-24.04`, `contents: read`, checkout и одна сборка `linux/amd64` с тегом `y2khub/language:latest`. Проверка: выполнить проверку синтаксиса workflow через `actionlint` и сверить trigger, платформу и команду сборки с дизайном.
- [x] 2.2 Оставить в workflow только checkout, login, build и push; убрать установку toolchain и отдельные тестовые шаги. Проверка: `actionlint` проходит, YAML содержит четыре шага без `continue-on-error`, а push выполняется только после успешного build.
- [x] 2.3 Добавить Docker login action с пользователем `y2khub` и `secrets.DOCKERHUB_TOKEN`, затем build и `docker push y2khub/language:latest` без повторной сборки. Дополнить README настройкой репозитория Docker Hub, имени секрета и права записи токена. Проверка: `actionlint` проходит; push выполняется после успешной сборки с тестами; токен используется только для login и отсутствует в build args и Dockerfile.
- [x] 2.4 Выполнять `opam exec -- make test` с временным `LY2K_PACKAGES_DIR` внутри builder и описать это в README. Проверка: полный suite eval/JS/Java успешен внутри Docker; сборка временного контекста с намеренно неверным ожидаемым выводом sample завершается ошибкой. Реальный запуск на GitHub проверяется задачей 3.2.

## 3. Интеграционная приёмка

- [x] 3.1 Проверить итоговую Docker-сборку с полным `make test` и сценарии контейнерной спецификации. Проверка: suites eval/JS/Java и локальные проверки финального контейнера проходят; `openspec validate add-docker-publishing --strict` успешен.
- [x] 3.2 После настройки владельцем репозитория `y2khub/language` и секрета `DOCKERHUB_TOKEN` проверить первый workflow на push в `main`: успешные login, build с тестами eval/JS/Java и push. Затем выполнить pull опубликованного образа и пример eval, сверить опубликованный digest с выводом push. Проверка: GitHub Actions run успешен, образ доступен из Docker Hub, запуск возвращает `Hello, world!`.

Подтверждение 3.2: [GitHub Actions run 37361973838](https://github.com/y2k/language/actions/runs/37361973838), commit `c7ba2be`, 392 успешных теста внутри Docker-сборки. Pull `y2khub/language:latest` и запуск default/eval успешны. Digest совпадает с выводом CI push: `sha256:00103857fc03f881fdd868a6cbe847927fd51cd59860d4f541aeead981b751f2`.

Статическая линковка с musl, проверка запуска в `scratch` и перенос одного самодостаточного бинарника относятся к отдельной будущей задаче, зафиксированной в `proposal.md`, и не входят в этот checklist.
