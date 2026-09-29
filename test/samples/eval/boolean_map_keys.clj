;; 1 2 3 4|1 2 3 4|1 2 3 4

(defn describe [{false f "false" fs true t "true" ts}]
  (str f " " fs " " t " " ts))

(defn test []
  (let [m (hash-map false 1 "false" 2 true 3 "true" 4)
        {false f "false" fs true t "true" ts} m]
    (str (get m false) " " (get m "false") " " (get m true) " " (get m "true") "|"
         f " " fs " " t " " ts "|" (describe m))))
