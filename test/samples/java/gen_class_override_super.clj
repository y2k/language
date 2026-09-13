;; 0/0

(gen-class
 :name Items
 :extends java.util.ArrayList
 :methods [[^override clear [] void]])

(def events (atom ""))

(defn -clear [^java.util.ArrayList this]
  (swap! events (fn [old] (str old (.size this) "/"))))

(defn test []
  (let [^java.util.ArrayList items (Items.)]
    (.add items "removed")
    (.clear items)
    (str (deref events) (.size items))))
