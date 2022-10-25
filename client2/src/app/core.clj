(ns app.core
  (:require [garden.core :as gc]
            [garden.selectors :as gs]))

(defn- elipsize [s max-len]
  (some-> s (subs  0 (min max-len (count s)))))

(defn- make-preview-url [url size]
  (str
   "https://rc.y2k.work/cache/fit?width=" size "&height=" size "&bgColor=ffffff&quality=75&url="
   (->
    (clojure.string/replace url #"[^/]+(-\d+\.\w+?)" "$1")
    (java.net.URLEncoder/encode "UTF-8"))))

(def baseUrl "https://joyreactor.cc")
(def storage ::db)

(declare download-next-page)

(defn load-next {:cofx [:db]} [db e]
  (download-next-page db))

(defn render-page [_ state]
  [[:ui [:html {}
         [:head {}
          [:title {:innerText "Posts"}]
          [:link {:rel "stylesheet" :href "https://necolas.github.io/normalize.css/8.0.1/normalize.css"}]
          [:script {:async true :src "https://unpkg.com/htmx.org@1.8.2"}]
          [:style
           {:innerText
            (gc/css
             [:body {:padding "16px" :font-family "Arial"}]
             [:.post-list {:display "flex" :flex-direction "column"}]
             [:.post-list [:* [(gs/& (gs/not (gs/first-child))) {:margin-top "8px"}]]]
             [:.post {:background "#f0f0f0"
                      :border-radius "4px"
                      :padding "8px"
                      :display "flex"
                      :flex-direction "column"}]
             [:.post [:* [(gs/& (gs/not gs/first-child)) {:margin-top "8px"}]]]
             [:.post__title {:font-size "18px" :font-weight 600}]
             [:.post__image {:aspect-ratio 1}]
             [:.button {:background "#f5d21c"
                        :border-color "transparent"
                        :border-radius "4px"
                        :font-size "16px"
                        :padding "8px"}]
             [:.button:hover {:background "#f5d21c"}]
             [:.button:active {:opacity "0.7"}])}]]
         [:body {}
          [:div {}
           [:h1 {:innerText "Posts"}]
           (concat
            [:div {:class "post-list"}
             [:button {:class "button" :innerText "Next" :onclick :load-next}]]
            (for [i (:items state)]
              [:div {:class "post"}
               [:span {:class "post__title" :innerText (elipsize (:innerText (:title i)) 100)}]
               [:img {:class "post__image" :loading "lazy" :src (make-preview-url (:src (:image i)) 200)}]])
            [[:button {:class "button" :innerText "Next" :onclick :load-next}]])]]]]])

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
