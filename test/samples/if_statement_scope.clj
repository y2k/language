;; true|true|true|true
(def x "global")
(defn test []
  (let [x "outer"
        value (str (let [x (str x "-inner")] (if true x "unused"))
                   (if true x "unused"))
        sibling (str (let [y "a"] y) (let [y "b"] (if true y "unused")))
        callback (let [x "captured"] (fn [] (if true x "unused")))]
    (str (= value "outer-innerouter") "|"
         (= sibling "ab") "|"
         (= (callback) "captured") "|"
         (= (str (let [x "local"] x) (if true x "unused")) "localouter"))))
