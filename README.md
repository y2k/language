# Компилируемый-встраиваемый LISP с интеропом

## Пример кода

```clojure
(module App

  (defn show [env text]
    (.show 
      (android.widget.Toast/makeText env.context text 0)))

  (defn main [env]
    (show env "Hello World")))
```
