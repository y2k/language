;; 1 2 2 1 1 1|false false true true|false true false true|7 false false yes|false

(defn test []
  (str (if "false" 1 2) " " (if false 1 2) " " (if nil 1 2) " "
       (if 0 1 2) " " (if "" 1 2) " " (if [] 1 2) "|"
       (not "false") " " (not "nil") " " (not false) " " (not nil) "|"
       (= false "false") " " (not= true "true") " " (= true false) " " (= true true) "|"
       (and "false" 7) " " (or false "false") " "
       (if-let [x "false"] x "other") " " (cond "false" "yes" :else "no") "|"
       (case "false" false "boolean" "false" "false" "missing")))
