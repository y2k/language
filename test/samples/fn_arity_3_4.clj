;; 1231234

(defn concat3 [a b c]
  (str a b c))

(defn test []
  (str (concat3 1 2 3)
       ((fn [a b c d] (str a b c d)) 1 2 3 4)))
