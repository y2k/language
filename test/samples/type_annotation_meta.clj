;; hello foo foo foo

(def ^String name "foo")

(defn greet [^String name]
  (str "hello " name))

(defn inline_fn []
  ((fn [^String name] name) "foo"))

(defn local_binding []
  (let [^String name "foo"]
    name))

(defn test []
  (str (greet name)
       " "
       (inline_fn)
       " "
       (local_binding)))
