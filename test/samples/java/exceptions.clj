;; true|true|true|true|true|true|true|true
(raw-code "static boolean identical(Object a, Object b) { return a == b; }")
(def error (new Exception "original"))
(def other (new Exception "cleanup"))
(defn raise [error] (throw error))
(defn test []
  (let [trace (atom "")
        success (try false (catch e true) (finally (reset! trace "f")))
        nil-result (try (throw error) (catch e nil))
        false-result (try (throw error) (catch e false))
        rethrown (try (try (throw error) (catch e (let [e "inner"] e) (throw e)))
                      (catch e (identical e error)))
        callback (try ((fn [] (raise error))) false (catch e (identical e error)))
        handler (try (try (throw error) (catch e (throw other))
                         (finally (swap! trace (fn [s] (str s "h")))))
                     (catch e (identical e other)))
        cleanup (try (try (throw error) (catch e (throw e)) (finally (throw other)))
                     (catch e (and (identical e other) (= (.getCause e) nil))))
        count (atom 0)
        once (try (str (throw (try (let [e error]
                                    (do (swap! count (fn [n] (+ n 1))) (if true e other)))
                                  (finally (swap! trace (fn [s] (str s "t")))))))
                  (catch e (and (identical e error) (= (deref count) 1))))
        stopped (try (do (throw error) (reset! count 42) 42) (catch e (= (deref count) 1)))]
    (str (and (= success false) (not success)) "|"
         (and (= nil-result nil) (not nil-result) (= false-result false) (not false-result)) "|"
         rethrown "|" callback "|" handler "|" cleanup "|" once "|"
         (and stopped (= (deref trace) "fht")))))
