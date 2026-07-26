## 1. Java Generator

- [x] 1.1 Обновить генерацию singleton `(let* NAME VALUE)` в `backend_compiler/java.ml`, чтобы `type_annotation` символьного binding оборачивал скомпилированный RHS в Java cast, сохраняя прежнюю генерацию для bindings без hint и однократное вычисление RHS.

## 2. Java End-to-End Coverage

- [x] 2.1 Добавить fixture в `test/samples/java/`, который получает `Object`-typed результат функции языка, связывает его с `^java.lang.String` и успешно вызывает Java method.
- [x] 2.2 Запустить `ocamlformat -i backend_compiler/java.ml` и полную проверку `make test`.
