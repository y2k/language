;; >123|empty

(defn join [a b] (str a b))
(defn test []
  (str
   (reduce join ">" [1 2 3])
   "|"
   (reduce join "empty" [])))
