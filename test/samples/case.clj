;; two,other

(defn test []
  (case 2
    1 "wrong"
    2 (case 3 1 "wrong" 2 "wrong" "two,other")
    "wrong"))
