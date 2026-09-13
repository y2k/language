## 1. Генерация методов

- [x] 1.1 Удалить безусловный `@Override` из `compile_gen_method` в `backend_compiler/java.ml`; обновить snapshot `gen_class` и добавить проверки нового метода и отказа для non-void результата в `test/java_ns_test.ml`. Выполнить `ocamlformat -i` для изменённых `.ml` файлов и проверить `dune exec ./test/java_ns_test.exe`: аннотация отсутствует, существующий `super.onCreate(arg0)` предшествует helper.

## 2. Проверки исполнения

- [x] 2.1 Добавить Java fixtures в `test/samples/java/` для вызова нового `void`-метода подкласса `Object` и передачи получателя и двух строковых аргументов. Проверить через существующий sample runner компиляцию `javac` и ожидаемый вывод, включающий результат проверки получателя и исходный порядок аргументов.
- [x] 2.2 Добавить Java fixtures переопределения конкретного метода стандартного предка с `^override` и без него, используя наблюдаемое состояние, например размер `ArrayList` при `clear`. Проверить вызов через тип предка: helper вызывается один раз, без metadata содержимое сохраняется, с metadata helper наблюдает уже очищенный список; проверку однократности вызова `super` дополнить проверкой generated source.
- [x] 2.3 Добавить небольшой целевой тест прямого entry point в существующую Java test infrastructure, переиспользуя запуск процессов из `test/test.ml`. На JDK 25 скомпилировать source из delta-spec вместе с runtime и запустить `java -cp out 'checks.entry$Runner'`; проверить вывод `ok` и exit code 0 без static wrapper и preview flags. Отсутствие JDK 25 не считать успешной проверкой сценария.
- [x] 2.4 Добавить отрицательную проверку нового метода `[^override main [] void]` у подкласса `Object`: убедиться, что generated source сохраняет `super.main()`, а `javac` завершается с ненулевым кодом из-за отсутствующего метода предка.

## 3. Справочник и интеграция

- [x] 3.1 Обновить английский раздел `gen-class` в `skills/y2k-language/SKILL.md`: новые и переопределяемые `void` instance methods, отсутствие `@Override`, независимость переопределения от вызова `super`, прежний смысл `^override`, пример прямого запуска вложенного класса на JDK 25. Сверить пример и ограничения с delta-spec и выполненным тестом entry point.
- [x] 3.2 Выполнить `ocamlformat -i` для всех изменённых `.ml`/`.mli`, затем `make test` на JDK 25; подтвердить успех полной сборки и тестов, включая новые Java regression checks, и зафиксировать результат проверки прямого launcher.

## Результаты проверки

- `ocamlformat -i backend_compiler/java.ml test/java_ns_test.ml test/test.ml` выполнен; `git diff --check` прошёл.
- `dune exec ./test/java_ns_test.exe`: 20 тестов прошли.
- Новые Java fixtures прошли с выводом `true true left right`, `1/1` и `0/0`.
- На OpenJDK 25.0.1 прямой запуск `checks.entry$Runner` с реализацией `(println "ok")` вывел `ok` и завершился с кодом 0, без wrapper и preview flags. Вариант с `^override main` сохранил `super.main()` и получил ожидаемый отказ `javac` с кодом 1 и диагностикой отсутствующего метода.
- `make test`: сборка и все suites прошли, включая 176 sample/entry-point тестов.
