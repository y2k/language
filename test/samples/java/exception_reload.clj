;; true|true|true
(def error (new Exception "reload"))
(def registry (atom "old"))
(def loading (atom false))
(def events (atom ""))
(defn note [event] (swap! events (fn [s] (str s event))))
(defn reload [fail]
  (reset! loading true)
  (try
    (let [new-value (do (note "init") (if fail (throw error) "new"))]
      (reset! registry new-value)
      (note "success")
      true)
    (catch e (note "failure") false)
    (finally (reset! loading false) (note "clear"))))
(defn test []
  (let [failed (reload true)
        retained (and (= failed false) (= (deref registry) "old") (not (deref loading))
                      (= (deref events) "initfailureclear"))]
    (note "next")
    (let [success (reload false)]
      (str retained "|" (and success (= (deref registry) "new") (not (deref loading))) "|"
           (= (deref events) "initfailureclearnextinitsuccessclear")))))
