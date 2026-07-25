## 1. Cross-Backend Проверка

- [x] 1.1 Добавить общий sample в `test/samples/`, который передаёт лямбду через параметр, вызывает её и проверяет lexical shadowing одноимённой runtime-функции.
- [x] 1.2 Подтвердить, что sample проходит на eval и JavaScript и до исправления воспроизводит ошибку Java compilation.

## 2. Java Generator

- [x] 2.1 Изменить Java generic call для локальной атомарной головы: выбрать `FnN` по числу аргументов, привести локальное значение и вызвать `.call(...)`, не меняя нелокальные прямые вызовы.
- [x] 2.2 Отформатировать изменённые `.ml`/`.mli` файлы командой `ocamlformat -i`.

## 3. Verification

- [x] 3.1 Запустить `make test` и подтвердить прохождение общего sample suite на eval, JavaScript и Java targets.
