;; true false true false true false true false

(defn test []
  (str
   (> 2 1) " " (> 1 2) " "
   (< 1 2) " " (< 2 1) " "
   (>= 2 2) " " (>= 1 2) " "
   (<= 2 2) " " (<= 2 1)))
