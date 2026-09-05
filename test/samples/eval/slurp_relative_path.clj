;; true

(defn test []
  (= (slurp "samples/eval/slurp_input.txt")
     "first line\nsecond line\n"))
