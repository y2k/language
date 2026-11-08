;; true|true|true|true|true|true|true|true|true|true
(def trace (atom ""))
(def error (new Error "stop"))
(defn record [s value] (swap! trace (fn [old] (str old s))) value)
(def first-value (try (record "init1" 1) (finally (record "clean1" nil))))
(def second-value (try (record "init2" first-value) (finally (record "clean2" nil))))
(defn consume [a b c] (record "call" (str a b c)))
(defn test []
  (let [initialized (= (deref trace) "init1clean1init2clean2")]
    (reset! trace "")
    (let [value (consume (record "before" 1) (try (record "work" 2) (finally (record "clean" nil))) (record "after" 3))
          ordered (and (= value "123") (= (deref trace) "beforeworkcleanaftercall"))]
      (reset! trace "")
      (let [early (try (consume (do (record "early" nil) (throw error))
                                (try (record "bad" 2) (catch e (record "bad" 3)) (finally (record "bad" nil)))
                                (record "bad" 4))
                       (catch e (= (deref trace) "early")))]
        (reset! trace "")
        (let [dynamic ((record "callee" (fn [a b] (record "call" (str a b))))
                       (try (record "one" 1) (finally (record "clean1" nil)))
                       (try (record "two" 2) (finally (record "clean2" nil))))
              two (and (= dynamic "12") (= (deref trace) "calleeoneclean1twoclean2call"))]
          (reset! trace "")
          (let [receiver (. (cast String (record "receiver" "a")) concat
                            (try (record "arg" "b") (finally (record "clean" nil))))
                host (and (= receiver "ab") (= (deref trace) "receiverargclean"))]
            (reset! trace "")
            (let [items [(record "a" 1) (try (record "b" 2) (finally (record "c" nil))) (record "d" 3)]
                  dic {:a (record "e" 4) :b (try (record "f" 5) (finally (record "g" nil)))}
                  collections (and (= (get items 1) 2) (= (:b dic) 5) (= (deref trace) "abcdefg"))]
              (reset! trace "")
              (if false (try (record "bad" 1) (finally (record "bad" nil))) nil)
              (let [later (fn [] (try (record "later" 1) (finally (record "clean" nil))))
                    deferred (= (deref trace) "")]
                (later)
                (let [e "outer"
                      inner (try (throw error) (catch e (let [e "inner"] e)))
                      captured (try (throw error) (catch e (let [e "inner"] e) (let [x 1] x) (let [x 2] x) (fn [] e))
                                    (finally (record e nil)))
                      scoped (and (= inner "inner") (= e "outer") (= (captured) error))
                      rethrow (try (try (throw error) (catch e (let [e "inner"] e) (throw e))) (catch e (= e error)))
                      java-meta ^void:java.lang.Runnable (fn [] nil)]
                  (str initialized "|" ordered "|" early "|" two "|" host "|" collections "|"
                       (and deferred (= (deref trace) "latercleanouter")) "|" scoped "|" rethrow "|" (= (java-meta) nil)))))))))))
