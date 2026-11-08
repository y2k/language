;; true|true|true|true|true|true
(def events (atom ""))
(def loading (atom false))
(def error (new Exception "resource"))
(defn note [s] (swap! events (fn [old] (str old s "/"))))
(defn step [stage name] (note name) (if (= stage name) (throw error) nil))
(defn resources [stage]
  (reset! loading true)
  (try
    (step stage "open1")
    (try
      (step stage "open2")
      (try
        (step stage "read")
        (step stage "decode")
        (step stage "EOF")
        (step stage "finish")
        (note "done")
        (finally (step stage "close2")))
      (finally (step stage "close1")))
    (finally (reset! loading false) (note "clear"))))
(defn check-case [stage expected]
  (reset! events "")
  (let [failed (try (resources stage) false (catch e (= e error)))]
    (and (= failed (not= stage "none")) (not (deref loading)) (= (deref events) expected))))
(defn test []
  (str (check-case "none" "open1/open2/read/decode/EOF/finish/done/close2/close1/clear/") "|"
       (check-case "read" "open1/open2/read/close2/close1/clear/") "|"
       (check-case "decode" "open1/open2/read/decode/close2/close1/clear/") "|"
       (check-case "finish" "open1/open2/read/decode/EOF/finish/close2/close1/clear/") "|"
       (check-case "open2" "open1/open2/close1/clear/") "|"
       (check-case "close2" "open1/open2/read/decode/EOF/finish/done/close2/close1/clear/")))
