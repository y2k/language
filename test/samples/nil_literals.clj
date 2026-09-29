;; true false true 2 1 true false

(defn test []
  (str (= nil nil) " "
       (= nil "nil") " "
       (not= nil "nil") " "
       (if nil 1 2) " "
       (if "nil" 1 2) " "
       (not nil) " "
       (not "nil")))
