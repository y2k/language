;; true|true,false|true,false|true,false

(defn test []
  (str
   (= nil nil) "|"
   (= true true) "," (= true false) "|"
   (= "a" "a") "," (= "a" "b") "|"
   (= 1 1) "," (= 1 2)))
