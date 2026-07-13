;; Ada-first-pair

(defn describe [{:name name :tags [tag]}]
  (str name "-" tag))

(defn test []
  (str (describe {:name "Ada" :tags ["first" "second"]}) "-" ((fn [[left]] left) ["pair" "ignored"])))
