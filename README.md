# Embedded LISP with Interop

### Usage Examples

- https://github.com/y2k/relax_cats_bot
- https://github.com/y2k/declarative_ban_bot
- https://github.com/y2k/charge_timer (compiled to Java for Android)
- https://github.com/y2k/declarative_notify

### Compilation Platforms

- JS
- Java
- Interpreter
- "Bytecode" for JVM Interpreter

### Code Example

```clojure
(ns app
 (:import [android.widget Toast]
 	        [android.content Context])

(defn- show [^Context context ^String text]
  (.show
    (Toast/makeText context text Toast.LENGTH_LONG)))

(defn main [^Context context]
  (show context "Hello World")))
```
