;; true|true|false|true|true|false
(defn test []
  (let [out (println "out")
        err (eprintln "err")]
    (str (= out nil) "|" (not= out "nil") "|" (if out true false)
         "|" (= err nil) "|" (not= err "nil") "|" (if err true false))))
