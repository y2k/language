;; 1/1

(gen-class
 :name Items
 :extends java.util.ArrayList
 :methods [[clear [] void]])

(def events (atom ""))

(defn -clear [^java.util.ArrayList this]
  (swap! events (fn [old] (str old (.size this) "/"))))

(defn test []
  (let [^java.util.ArrayList items (Items.)]
    (.add items "kept")
    (.clear items)
    (str (deref events) (.size items))))
