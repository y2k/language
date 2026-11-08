;; true|true|true|true|true|true|true|true
(def trace (atom ""))
(defn record [tag value]
  (swap! trace (fn [s] (str s tag)))
  value)
(defn finish [a b c] (record "f" (str a b c)))
(def first-value (if (record "D" true) (record "1" nil) (record "X" 1)))
(def second-value (if (= first-value nil) (record "2" false) (record "X" true)))

(defn test []
  (let [defs (and (= (deref trace) "D12") (= first-value nil) (= second-value false))]
    (reset! trace "")
    (let [value (finish (record "before," "a")
                        (if (record "condition," true) (record "selected," "b") (record "unused," "x"))
                        (record "after," "c"))
          order (and (= value "abc") (= (deref trace) "before,condition,selected,after,f"))]
      (reset! trace "")
      (let [value ((do (record "callee," nil) finish)
                   (if (record "c1," true) (record "s1," "a") (record "x," "x"))
                   (if (record "c2," false) (record "x," "x") (record "s2," "b"))
                   (record "after," "c"))
            dynamic (and (= value "abc") (= (deref trace) "callee,c1,s1,c2,s2,after,f"))]
        (reset! trace "")
        (let [v [(record "a" nil) (if (record "b" true) (record "c" false) true) (record "d" 3)]
              vector-order (and (= (deref trace) "abcd") (= (get v 0) nil) (= (get v 1) false))]
          (reset! trace "")
          (let [v (list (record "a" nil) (if (record "b" true) (record "c" false) true))
                list-order (and (= (deref trace) "abc") (= (get v 0) nil) (= (get v 1) false))]
            (reset! trace "")
            (let [v {(record "a" :key) (record "b" nil)
                     (record "c" :other) (if (record "d" true) (record "e" false) true)}
                  map-order (and (= (deref trace) "abcde") (= (get v :key) nil) (= (get v :other) false))
                  callback (fn [] (if (record "later," true) (record "called" false) nil))
                  deferred (= (deref trace) "abcde")]
              (reset! trace "")
              (if false (if (record "unused" true) (record "unused" nil) nil) nil)
              (let [value (callback)
                    branches (and (= value false) (= (if false 1) nil) (= (deref trace) "later,called"))]
                (str defs "|" order "|" dynamic "|" vector-order "|" list-order "|" map-order "|" deferred "|" branches)))))))))
