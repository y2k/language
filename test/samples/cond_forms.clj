;; true|true|true|true|true|true|true|true

(defn test []
  (let [nested (cond false 0 true (cond nil 1 :else 42))
        vector (cond true [(cond false 0 :else 7)])
        mapping (cond false {:answer 0} :else {:answer 42})
        bound (cond true (let [[x] [42]] x))
        text (cond false "unused" :else "a\"b\\c\nd")]
    (str (= nested 42) "|"
         (= (get vector 0) 7) "|"
         (= (get mapping :answer) 42) "|"
         (= bound 42) "|"
         (= text "a\"b\\c\nd") "|"
         (= (cond true false :else (println "unreachable")) false) "|"
         (= (cond true nil :else (println "unreachable")) nil) "|"
         (not= (cond) "nil"))))
