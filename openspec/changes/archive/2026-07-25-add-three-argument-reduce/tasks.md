## 1. Runtime Support

- [x] 1.1 Расширить `prelude/language_runtime.js`, чтобы `reduce` различал двух- и трёхаргументную формы и использовал `init` для полной свёртки списка.
- [x] 1.2 Добавить в `prelude/language_runtime.java` overload `reduce(Object fn, Object init, Object collection)` с возвратом `init` для пустого списка.

## 2. Cross-Backend Verification

- [x] 2.1 Добавить общий sample-тест для непустого и пустого списка с `(reduce fn init collection)`, сохранив существующую проверку двухаргументной формы.
- [x] 2.2 Запустить `make test` и убедиться, что sample проходит на `eval`, `js` и `java` targets.
