;; true|true|true|true|true|true
(defn test []
  (let [messages [{:type "text" :id 1} {:type "image" :id 2} {:type "text" :id 3}]
        selected (filter (fn [message] (= (:type message) "text")) messages)
        values (filter (fn [x] x) [nil false 0 "" "nil" "false"])
        all (filter (fn [x] true) (list nil false))
        none (filter (fn [x] false) messages)]
    (str (and (= (count selected) 2) (= (:id (get selected 0)) 1) (= (:id (get selected 1)) 3)) "|"
         (and (= (count values) 4) (= (get values 0) 0) (= (get values 1) "")
              (= (get values 2) "nil") (not= (get values 2) nil) (= (get values 3) "false")) "|"
         (and (= (count all) 2) (= (get all 0) nil) (= (get all 1) false)) "|"
         (and (vector? none) (= (count none) 0) (not= none nil)) "|"
         (and (= (count messages) 3) (= (:id (get messages 0)) 1)
              (= (:type (get messages 1)) "image") (= (:id (get messages 2)) 3)) "|"
         (= (count (filter (fn [x] true) [])) 0))))
