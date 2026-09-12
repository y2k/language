;; nil nil 42 nil nil nil 42 false 42 nil false 42 42 nil 7 nil 8 nil false

(defn path [] [:chat :id])

(defn test []
  (let [data {:chat {:id 42}}
        keys [:chat :id]]
    (str (get nil :text) " "
         (:text nil) " "
         (get-in data [:chat :id]) " "
         (get-in {} [:chat :id]) " "
         (get-in {:chat nil} [:chat :id]) " "
         (get-in nil [:chat :id]) " "
         (get-in {:items [{:id 42}]} [:items 0 :id]) " "
         (get-in {:enabled false} [:enabled]) " "
         (get (get-in {:id 42} []) :id) " "
         (get-in nil []) " "
         (get-in false []) " "
         (get-in data keys) " "
         (get-in data (path)) " "
         (get-in {:items []} [:items 0 :id]) " "
         (get {:id 7} :id) " "
         (get {} :id) " "
         (get [8] 0) " "
         (get [8] 1) " "
         (get {:enabled false} :enabled))))
