;; 0.75 -0.25 0.125|1.25 0.75 0.5|1.75 0.25 -0.25|1.5 1|0.25 0.25 0.25|1.2 0.4|1 true false|1 true false|1 true false|-1 true false|0 true false

(defn describe-result [value expected]
  (str value " " (= value expected) " " (not= value expected)))

(defn test []
  (str (+ 0.25 0.5) " " (- 0.25 0.5) " " (* 0.5 0.25) "|"
       (+ 1 0.25) " " (- 1 0.25) " " (* 2 0.25) "|"
       (+ 0.25 0.5 1) " " (- 1 0.25 0.5) " " (* -2 0.5 0.25) "|"
       (* (+ 0.25 0.5) 2) " " (reduce (fn [a b] (+ a b)) 0 [0.25 0.5 0.25]) "|"
       (+ 0.25) " " (* 0.25) " " (- 0.25) "|"
       (+ 1 0.2) " " (* 2 0.2) "|"
       (describe-result (+ 0.5 0.5) 1) "|"
       (describe-result (- 1.5 0.5) 1) "|"
       (describe-result (* 2 0.5) 1) "|"
       (describe-result (- 0.5 1.5) -1) "|"
       (describe-result (* -0.5 0) 0)))
