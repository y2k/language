;;

(module

 (defn main []
   (reg-event
    :remove-item [:now]
    (fn [coeffects x]
      (let [db (:db coeffects)
            now (:now coeffects)]
        {:db (assoc db :items (dissoc (:items db) x))})))

   (reg-event
    :add-item []
    (fn [coeffects _]
      (let [db (:db coeffects)]
        {:db (assoc (assoc db :text "") :items (conj (:items db) (:text db)))})))

   (reg-event
    :text-changed []
    (fn [coeffects new-text]
      {:db (assoc (:db coeffects) :text new-text)})))

 (defn view []
   [:column
    [:edit-text {:text :text :on-change [:text-changed]}]
    [:button {:text "Add" :on-click [:add-item]}]
    [:template
     {:component [:column]
      :items :items
      :template
      [:column
       [:label {:text :title}]
       [:button
        {:text "Remove"
         :on-click [:remove-item]}]]}]]))

;;

(defn init [] {:text "" :items []})

(defn text-changed [coeffects new-text]
  {:db (assoc (:db coeffects) :text new-text)})

(defn add-item [coeffects _]
  (let [db (:db coeffects)]
    {:db (assoc db :items (conj (:items db) (:text db)))}))

(defn remove-item [coeffects x]
  (let [db (:db coeffects)]
    {:db (assoc db :items (dissoc (:items db) x))}))
