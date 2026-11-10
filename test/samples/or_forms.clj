;; true|true|true|true|true|true|true|true|true|true|true|true|true|true|true|true

(defn record [events label value]
  (swap! events (fn [text] (str text label)))
  value)

(defn test []
  (let [events (atom "")
        items [1 2]
        selected (or (record events "a" nil)
                     (record events "b" false)
                     (record events "c" items)
                     (record events "unused" 42))
        single (or (record events "d" 7))
        last-false (or (record events "e" nil) (record events "f" false))
        last-nil (or (record events "g" false) (record events "h" nil))
        nested (or (and true (or nil false)) (case (or false 7) 7 (or nil 42) 0))]
    (str (= selected items) "|"
         (= single 7) "|"
         (= last-false false) "|"
         (not last-false) "|"
         (= last-nil nil) "|"
         (not= last-nil "nil") "|"
         (not last-nil) "|"
         (= (deref events) "abcdefgh") "|"
         (= nested 42) "|"
         (= (or) nil) "|"
         (= (or nil) nil) "|"
         (= (or false) false) "|"
         (= (or "false" 42) "false") "|"
         (= (or "nil" 42) "nil") "|"
         (if (or "false" 42) true false) "|"
         (if (or "nil" 42) true false))))
