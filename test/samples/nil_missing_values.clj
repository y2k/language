;; true:true true:true true:true true:true true:true true:true

(defn describe-missing [value]
  (str (= value nil) ":" (not= value "nil")))

(defn missing-leaf [[first] {:value value}]
  (str (describe-missing first) " " (describe-missing value)))

(defn test []
  (let [nested (get-in {:items [{}]} [:items 0 :missing :leaf])
        indexed (get [] 0)
        [first] []
        {:missing value} {}
        argument (missing-leaf [] {})]
    (str (describe-missing nested) " " (describe-missing indexed) " "
         (describe-missing first) " " (describe-missing value) " " argument)))
