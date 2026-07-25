## 1. Regression Проверка

- [x] 1.1 Использовать расширенный `test/samples/function_parameter_call.clj` для проверки вызова функции из созданного destructuring local binding и подтвердить исходное падение только Java target.

## 2. Java Generator

- [x] 2.1 Последовательно передавать обновлённый `ctx.locals` между формами тел top-level функций, lambdas и lexical blocks, добавляя singleton `let*` binding только после компиляции его RHS.
- [x] 2.2 Сохранить границы lexical scope и существующий dispatch нелокальных runtime, top-level и qualified calls.
- [x] 2.3 Отформатировать изменённые `.ml`/`.mli` файлы командой `ocamlformat -i`.

## 3. Verification

- [x] 3.1 Запустить `make test` и подтвердить прохождение общего sample suite на eval, JavaScript и Java targets.
