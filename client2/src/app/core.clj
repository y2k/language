(ns app.core)

(def baseUrl "https://joyreactor.cc")
(def storage ::db)

(declare download-next-page)

(defn load-next {:cofx [:db]} [db e]
  (download-next-page db))

(defn render-page [_ state]
  [[:ui [:div {}
         [:div {:innerText "Posts"}]
         (for [i (:items state)]
           [:div {}
            [:div {:className "title" :innerText (:innerText (:title i))}]
            [:img {:src (:src (:image i))}]])
         [:button {:className "button" :innerText "Next" :onclick :load-next}]]]])

(defn page-loaded {:cofx [:db]} [{db :db} page]
  [[:parse-html {:page page
                 :target {:items {:type :collection
                                  :query "div.postContainer"
                                  :item {:title {:type :node :query "div.post_content"}
                                         :image {:type :node :query "div.image img"}}}}
                 :dispatch render-page}]
   [:db (update db storage (fn [db] (assoc db :next 2)))]])

(defn download-next-page [db]
  [[:download-http {:url (str baseUrl (:next (storage db)))
                    :callback page-loaded}]])

(defn main {:cofx [:db]} [{db :db}]
  (download-next-page db))
