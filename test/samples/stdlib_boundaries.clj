;; 0 1 2 2 0 0 0 0|3 -3 5 0|nil nil 10 0|0 7 42

(defn test []
  (str (+) " " (*) " " (- 2) " " (/ 2) " " (/ 0) " "
       (count (list)) " " (count (concat)) " " (count (hash-map)) "|"
       (/ 7 2) " " (/ -7 2) " " (/ 20 2 2) " " (/ 0) "|"
       (get [10] 5) " " (get [] 0) " " (get (drop -1 [10]) 0) " "
       (count (drop 5 [10])) "|"
       (count (map (fn [x] x) [])) " "
       (reduce (fn [a b] (+ a b)) 7 []) " "
       (reduce (fn [a b] (+ a b)) [42])))
