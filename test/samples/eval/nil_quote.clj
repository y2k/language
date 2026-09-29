;; true true false true false

(defn test []
  (str (= 'nil nil) " "
       (= (quote nil) nil) " "
       (= '"nil" nil) " "
       (= (get '(nil "nil") 0) nil) " "
       (= (get '(nil "nil") 1) nil)))
