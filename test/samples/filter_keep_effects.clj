;; true|true|true|true|true|true|true|true
(ns checks.selection.core)

(def effects (atom ""))

(defn record [item]
  (swap! effects (fn [text] (str text item)))
  (if (= item 2) nil (+ item 10)))

(defn test []
  (reset! effects "")
  (let [items [1 2 3]
        selected (filter record items)
        filter-order (= (deref effects) "123")
        kept (keep record items)
        keep-order (= (deref effects) "123123")
        local record
        local-filter (filter local (list 4))
        local-keep (keep local (list 5))
        lambda-filter (filter (fn [x] (record x)) [6])
        lambda-keep (keep (fn [x] (record x)) [7])
        empty-filter (filter record [])
        empty-keep (keep record (list))]
    (str filter-order "|" keep-order "|"
         (and (= (count selected) 2) (= (get selected 0) 1) (= (get selected 1) 3)) "|"
         (and (= (count kept) 2) (= (get kept 0) 11) (= (get kept 1) 13)) "|"
         (and (= (get local-filter 0) 4) (= (get local-keep 0) 15)
              (= (get lambda-filter 0) 6) (= (get lambda-keep 0) 17)) "|"
         (= (deref effects) "1231234567") "|"
         (and (vector? empty-filter) (vector? empty-keep)
              (= (count empty-filter) 0) (= (count empty-keep) 0)) "|"
         (and (= (count items) 3) (= (get items 0) 1) (= (get items 1) 2) (= (get items 2) 3)))))
