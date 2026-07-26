;; foo!

(defn make-text []
  "foo")

(defn test []
  (let [^java.lang.String text (make-text)]
    (.concat text "!")))
