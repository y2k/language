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
          [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
          [:link {:rel "stylesheet" :href "https://necolas.github.io/normalize.css/8.0.1/normalize.css"}]
          [:script {:async true :src "https://unpkg.com/htmx.org@1.8.2"}]
          [:style
           {:innerText
            (gc/css
             [:body {:padding "0px 16px 16px" :font-family "Arial" :background "#1f1f1f" :color "white"}]
             [:.v_divider8 [:> [:* [(gs/& (gs/not gs/first-child)) {:margin-top "8px"}]]]]
             [:.v_divider16 [:> [:* [(gs/& (gs/not gs/first-child)) {:margin-top "16px"}]]]]
             [:.h_divider8 [:> [:* [(gs/& (gs/not gs/first-child)) {:margin-left "8px"}]]]]
             [:.h_divider16 [:> [:* [(gs/& (gs/not gs/first-child)) {:margin-left "16px"}]]]]
             [:.collapse {:display "none !important"}]
             [:.column {:display "flex" :flex-direction "column"}]
             [:.row {:display "flex" :flex-direction "row"}]
             [:.col_center {:justify-content "center"}]
             [:.row_center {:align-items "center"}]
             [:.post__description {}]
             [:.post__title {:color "white" :font-size "18px" :-webkit-line-clamp 3 :display "-webkit-box" :-webkit-box-orient "vertical" :overflow "hidden"}]
             [:.post__image {:aspect-ratio 1.3 :object-fit "cover" :border-radius "4px"}]
             [:.post__user_name {:font-size "14px" :color "#afafaf"}]
             [:.post__user_image {:width "25px" :height "25px" :border-radius "50%"}]
             [:.post__divider {:height "1px" :background "#3f3f3f"}]
             [:.button {:background "#f5d21c"
                        :border-radius "4px"
                        :border-color "transparent"
                        :font-size "16px"
                        :padding "8px"
                        :min-height "48px"}]
             [:.button:hover {:background "#f5d21c"}]
             [:.button:active {:opacity "0.7"}])}]]
         [:body {}
          [:div {}
           [:h1 {:innerText "Posts"}]
           (concat
            [:div {:class "column v_divider16"}]
            (for [i (:items state)]
              [:div {:class "post column v_divider16"}
               [:img {:class "post__image" :loading "lazy" :src (make-preview-url (:src (:image i)) 300)}]
               (let [title (:innerText (:title i))]
                 [:span {:class (str "post__title" (if (or (nil? title) (= "" title)) " collapse" ""))
                         :innerText (elipsize title 50)}])
               [:div {:class "post__description row h_divider8 row_center"}
                [:img {:class "post__user_image" :loading "lazy" :src (make-preview-url (:src (:user-image i)) 50)}]
                [:span {:class "post__user_name" :innerText (:innerText (:username i))}]]
               [:div {:class "post__divider"}]])
            [[:button {:class "button" :innerText "Next" :onclick "alert('test')"}]])]]]]])

(defn page-loaded {:cofx [:db]} [{db :db} page]
  [[:parse-html {:page page
                 :target {:items {:type :collection
                                  :query "div.postContainer"
                                  :item {:title {:type :node :query "div.post_content"}
                                         :username {:type :node :query "div.uhead_nick"}
                                         :user-image {:type :node :query "img.avatar"}
                                         :image {:type :node :query "div.image img"}}}}
                 :dispatch render-page}]
   [:db (update db storage (fn [db] (assoc db :next 2)))]])

(defn download-next-page [db]
  [[:download-http {:url (str baseUrl (:next (storage db)))
                    :callback page-loaded}]])

(defn main {:cofx [:db]} [{db :db}]
  (download-next-page db))
