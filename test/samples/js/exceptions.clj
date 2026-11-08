;; true|true|true|true|true|true|true|true|true
(raw-code "function identical(a, b) { return a === b; }
function hostFail(e) { throw e; }
const promised = Promise.reject('later'); promised.catch(() => {});")
(def error (new Error "original"))
(def other (new Error "cleanup"))
(defn test []
  (let [trace (atom "")
        success (try 1 42 (catch e 0) (finally (swap! trace (fn [s] (str s "f")))))
        nil-result (try (throw error) (catch e nil))
        false-result (try (throw error) (catch e false))
        rethrown (try (try (throw error) (catch e (throw e))) (catch e (identical e error)))
        host (try (hostFail error) false
                  (catch e (console/error e) (and (identical e error) (= (get e "message") "original"))))
        callback (try ((fn [] (throw error))) false (catch e (identical e error)))
        handler (try (try (throw error) (catch e (throw other))
                         (finally (swap! trace (fn [s] (str s "h")))))
                     (catch e (identical e other)))
        cleanup (try (try (throw error) (catch e (throw e)) (finally (throw other)))
                     (catch e (identical e other)))
        falsey (and (try (throw false) (catch e (identical e false)))
                    (try (throw nil) (catch e (identical e nil)))
                    (try (throw "text") (catch e (identical e "text"))))
        promise (try promised (catch e false) (finally (swap! trace (fn [s] (str s "p")))))
        count (atom 0)
        once (try (str (throw (try (let [e error] (swap! count (fn [n] (+ n 1))) e)
                                  (finally (swap! trace (fn [s] (str s "t")))))))
                  (catch e (and (identical e error) (= (deref count) 1))))]
    (str (= success 42) "|"
         (and (= nil-result nil) (not nil-result) (= false-result false) (not false-result)) "|"
         rethrown "|" host "|" callback "|" handler "|" cleanup "|" falsey "|"
         (and (identical promise promised) once (= (deref trace) "fhpt")))))
