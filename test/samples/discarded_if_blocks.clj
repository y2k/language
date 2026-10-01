;; true|true
(defn test []
  (let [counter (atom 0)]
    (if false (do (reset! counter 100) nil) nil)
    (if true (do (reset! counter 1) nil) nil)
    (let [selected (= (deref counter) 1)]
      (if false nil (do (reset! counter 2) nil))
      (str selected "|" (= (deref counter) 2)))))
