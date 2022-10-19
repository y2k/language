(ns app)

(def storage ::db)

(defn download-next-page [db]
  [[:download-http {:url (str "https://a.com/t/" (:next (get-in db storage)))
                    :dispatch :page-loaded}]
   [:db (update db storage (fn [db] (assoc db :next 2)))]])

(defn main {:cofx [:db]} [{db :db}]
  (download-next-page db))

(defn page-loaded {:cofx [:db]} [{db :db} page]
  [[:parse-html {:page page
                 :target {:items {:type :collection
                                  :query "div.thread"
                                  :item {:title {:type :node :query ".title"}
                                         :body {:type :node :query ".body"}}}
                          :title {:type :node :query "body > div.title"}}
                 :dispatch :render-page}]
   [:db {}]])

(defn render-page [_ state]
  [[:ui [:div {}
         [:div {:innerText "Posts"}]
         (for [i (:items state)]
           [:div {}
            [:div {:className "title" :innerText (:title i)}]
            [:div {:className "body" :innerText (:body i)}]])
         [:button {:className "button" :innerText "Next" :onclick :load-next}]]]])

(defn load-next {:cofx [:db]} [db e]
  (download-next-page db))
