;; true|true|true|true|true|true|true
(def initialized (try (Class/forName "no.such.Class") (catch e "recovered") (finally nil)))
(defn result [] (try 1 2 (finally nil)))
(defn test []
  (try 1 (finally nil))
  (let [x (try 3 (finally nil))
        callback (fn [] (try nil (finally nil)))
        nested (if true (do (let [y 4] (try y (finally nil)))) 0)]
    (str (= initialized "recovered") "|" (= (result) 2) "|"
         (= (str (try 1 (finally nil))) "1") "|"
         (= (if (try false (finally nil)) 1 2) 2) "|"
         (= x 3) "|" (= (callback) nil) "|" (= nested 4))))
