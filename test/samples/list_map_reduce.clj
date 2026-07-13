;; 112233

(defn join [a b] (str a b))
(defn dup [x] (str x x))
(defn test []
  (str
   (reduce
    join
    (map dup [1 2 3]))))
