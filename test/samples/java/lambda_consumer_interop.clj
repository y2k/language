;; (item)

(defn test []
  (let [xs (java.util.ArrayList.)
        add ^void:java.util.function.Consumer
        (fn [value]
          (.add xs value))]
    (.accept add "item")
    xs))
