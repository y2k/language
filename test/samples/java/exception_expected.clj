;; true|true|true|true
(defn throws? [f] (try (f) false (catch e true)))
(defn test []
  (let [events (atom "")
        normal-nil (throws? (fn [] nil))
        normal-false (throws? (fn [] false))
        raised (throws? (fn [] (throw (new Exception "expected"))))
        next (throws? (fn [] (reset! events "continued") nil))]
    (str (and (= normal-nil false) (not normal-nil)) "|"
         (and (= normal-false false) (not normal-false)) "|"
         (and (= raised true) raised) "|" (and (= next false) (= (deref events) "continued")))))
