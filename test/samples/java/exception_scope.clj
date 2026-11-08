;; true|true|true|true
(raw-code "static final Exception failure = new Exception(\"original\");
static Object fail(Object e) throws Exception { throw (Exception) e; }
static boolean identical(Object a, Object b) { return a == b; }")
(defn test []
  (let [e "outer"
        trace (atom "")
        inner (try (fail failure) (catch e (let [e "inner"] e)))
        captured (try (fail failure)
                      (catch e (let [e "inner"] e) (let [x 1] x) (let [x 2] x) (fn [] e))
                      (finally (reset! trace e)))
        rethrow (try (try (fail failure) (catch e (let [e "inner"] e) (fail e)))
                     (catch e (identical e failure)))
        message (try (fail failure) (catch e (.printStackTrace e) (.getMessage e)))]
    (str (= inner "inner") "|" (identical (captured) failure) "|" rethrow "|"
         (and (= e "outer") (= (deref trace) "outer") (= message "original")))))
