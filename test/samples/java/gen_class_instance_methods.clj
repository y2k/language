;; true true left right

(gen-class
 :name Runner
 :extends Object
 :methods [[main [] void] [accept [String String] void]])

(def entered (atom false))
(def received (atom nil))

(defn -main [this]
  (reset! entered true))

(defn -accept [this left right]
  (reset! received [this left right]))

(defn test []
  (let [runner (Runner.)]
    (.main runner)
    (.accept runner "left" "right")
    (let [args (deref received)]
      (str (deref entered) " " (.equals runner (get args 0))
           " " (get args 1) " " (get args 2)))))
