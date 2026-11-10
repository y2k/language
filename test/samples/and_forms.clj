;; true|true|true|true|true|true|true|true|true|true|true|true|true|true

(defn record [events label value]
  (swap! events (fn [text] (str text label)))
  value)

(defn test []
  (let [events (atom "")
        values [1 2]
        mapping {:answer 42}
        stopped-false (and (record events "a" true)
                           (record events "b" false)
                           (record events "unused" 42))
        stopped-nil (and (record events "c" nil)
                         (record events "unused" 42))
        single (and (record events "d" 7))
        last (and (record events "e" "false")
                  (record events "f" "nil")
                  (record events "g" values))
        nested (and (or false (and true 3))
                    (case (and true 7) 7 (and true 42) 0))]
    (str (= stopped-false false) "|"
         (= stopped-nil nil) "|"
         (not= stopped-nil "nil") "|"
         (= single 7) "|"
         (= last values) "|"
         (= (deref events) "abcdefg") "|"
         (= nested 42) "|"
         (= (and) true) "|"
         (= (and nil) nil) "|"
         (= (and false) false) "|"
         (= (and "false" "nil" 42) 42) "|"
         (= (and true nil) nil) "|"
         (= (and true false) false) "|"
         (= (and true mapping) mapping))))
