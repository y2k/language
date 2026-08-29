;; true true false false true

(defn nonzero? [value]
  (not (= value 0)))

(defn test []
  (str
   (not false) " "
   (not nil) " "
   (not 0) " "
   (nonzero? 0) " "
   (nonzero? 1)))
