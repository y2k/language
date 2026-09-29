;; true false 2 nil false true 1 true true nil

(defn identity-value [value]
  value)

(defn test []
  (let [cell (atom (identity-value nil))
        initial (deref cell)
        text (reset! cell "nil")
        cleared (swap! cell (fn [old]
                              (if (= old "nil")
                                (identity-value nil)
                                "unexpected value")))]
    (str (= initial nil) " " (= initial "nil") " "
         (if initial 1 2) " " initial " "
         (= text nil) " " (= text "nil") " "
         (if text 1 2) " " (= cleared nil) " "
         (= (deref cell) nil) " " (deref cell))))
