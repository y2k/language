## 1. Генерация import

- [x] 1.1 Применить `Symbol_munge.munge` к локальному алиасу string-require в `backend_compiler/js.ml` и убедиться, что module specifier `"node:async_hooks"` не изменяется.
- [x] 1.2 Добавить в `test/js_ns_test.ml` регрессионный случай для `:as async-hooks`, ожидающий `import * as async_hooks`, и проверить `dune exec ./test/js_ns_test.exe`.

## 2. Проверка

- [x] 2.1 Запустить `make test` и убедиться, что полный набор тестов проходит.
