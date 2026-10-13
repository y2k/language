;; true|true|true|true|true
(defn test []
  (let [events [{:id 1 :seen false} {:id 2 :seen true} {:id 3 :seen false}]
        cards (keep (fn [event] (if (:seen event) nil {:card (:id event)})) events)
        values (keep (fn [x] x) (list nil false 0 "" "nil" "false"))
        none (keep (fn [x] nil) events)]
    (str (and (= (count cards) 2) (= (:card (get cards 0)) 1) (= (:card (get cards 1)) 3)) "|"
         (and (= (count values) 5) (= (get values 0) false) (= (get values 1) 0)
              (= (get values 2) "") (= (get values 3) "nil") (not= (get values 3) nil)
              (= (get values 4) "false")) "|"
         (and (vector? none) (= (count none) 0) (not= none nil)) "|"
         (and (= (count events) 3) (= (:id (get events 0)) 1)
              (= (:seen (get events 1)) true) (= (:id (get events 2)) 3)) "|"
         (= (count (keep (fn [x] x) [])) 0))))
