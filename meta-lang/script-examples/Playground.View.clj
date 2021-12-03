;;

(module
 (:require "gist:y2k/5cee7ac1210474b3502fa118970f745d/a8d0a391c671234cb424fcb4743e6da47adf5a5a" :as dispatch)

 (defn view [model]
   [:column
    [:edit-text {:text (:text model) :on-change (fn [e] (dispatch [:text-changed e]))}]
    [:button {:text "Add" :on-click (fn [_] (dispatch [:add-item]))}]
    [:template
     {:component [:column]
      :items (:items model)
      :template (fn [model]
                  [:column
                   [:label {:text {:title model}}]
                   [:button
                    {:text "Remove"
                     :on-click (fn [_] (dispatch [:remove-item]))}]])}]])

 (defn main []
   (reg-event
    :init []
    (fn [coeffects _]
      {:text "" :items []}))

   (reg-event
    :remove-item [:now]
    (fn [coeffects x]
      (let [db (:db coeffects)
            now (:now coeffects)]
        {:db (assoc db :items (filter (fn [i] (not= x i)) (:items db)))})))

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
    [:edit-text {:text [:model :text] :on-change [:text-changed]}]
    [:button {:text "Add" :on-click [:add-item]}]
    [:template
     {:component [:column]
      :items [:model :items]
      :template [:column
                 [:label {:text [:model :title]}]
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
