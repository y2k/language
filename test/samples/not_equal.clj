;; false false true false true false true

(defn test []
  (str
   (not= nil nil) " "
   (not= true true) " " (not= true false) " "
   (not= "a" "a") " " (not= "a" "b") " "
   (not= 1 1) " " (not= 1 2)))
