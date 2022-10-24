(ns app.research-framework
  (:require [app.core :as app]
            [clojure.pprint :as pp]
            [org.httpkit.server :as hk]))

(comment

  (stop)

  (def stop (hk/run-server #'web-handler {:port 8080}))

  (run-with-cofx app/main)

  (require '[clojure.reflect :as r])
  (require '[clojure.pprint :as pp])
  (pp/pprint (r/reflect "hello"))

  (comment))

(defn render-to-html [[tag attr & children]]
  (str "<" (name tag)
       (clojure.string/join (map (fn [[k v]] (str " " (name k) "=\"" v "\"")) (dissoc attr :innerText)))
       ">"
       (:innerText attr) (clojure.string/join (map render-to-html children))
       "</" (name tag) ">"))

(def db (atom {}))

;; DONT DELETE !!!!!!!!!!!!!!!!!
;; (defn download-http [url]
;;   (->
;;    (java.net.http.HttpClient/newHttpClient)
;;    (.send (->
;;            (java.net.http.HttpRequest/newBuilder)
;;            (.uri (java.net.URI/create url))
;;            (.build))
;;           (java.net.http.HttpResponse$BodyHandlers/ofString (java.nio.charset.Charset/forName "UTF-8")))
;;    (.body)))

(defn download-http [url]
  (slurp "/Users/igor/Downloads/example_html.html"))

(declare execute-command)

(defn run-with-cofx-2 [f arg]
  (let [cofx {}
        commands (f cofx arg)]
    (doseq [c commands]
      (execute-command c))))

(defn- add-scheme [url]
  (if (or (nil? url) (= "" url))
    nil
    (str (.resolve (java.net.URI/create "https://site.org") url))))

(defn- create-vnode [node]
  (defn- add-some [target key value]
    (if (or (nil? value) (= "" value))
      target
      (assoc target key value)))
  (->
   {}
   (add-some :src (some-> node (.attr "src") (add-scheme)))
   (add-some :innerText (.text node))))

(defn make-node [jnode node-desc]
  (case (:type node-desc)
    (:collection)
    (let [nodes (.select jnode (:query node-desc))]
      (map (fn [n]
             (into {} (map (fn [[k v]] [k (make-node n v)]) (:item node-desc)))) nodes))
    (:node)
    (create-vnode (.selectFirst jnode (:query node-desc)))))

(defn parse-html [html nodes]
  (into {} (map (fn [[k v]] [k (make-node (org.jsoup.Jsoup/parse html) v)]) nodes)))

(comment

  (->
   (parse-html
    (slurp "/Users/igor/Downloads/example_html.html")
    {:items {:type :collection
             :query "div.postContainer"
             :item {:title {:type :node :query "div.post_content"}
                    :image {:type :node :query "div.image img"}}}})
   (pp/pprint))

  (parse-html
   (slurp "/Users/igor/Downloads/example_html.html")
   {:foo {:type :node :query "div.post_content"}})

  ())

(defn execute-command [[cmd arg]]
  (case cmd
    :parse-html (let [callback (:dispatch arg)
                      result (parse-html (:page arg) (:target arg))]
                  (run-with-cofx-2 callback result))
    :download-http (let [page (download-http (:url arg))
                         callback (:callback arg)]
                     (run-with-cofx-2 callback page))
    :db (reset! db arg)
    :ui (println (render-to-html arg))
    (throw (Exception. (str "Cant'find effect handler for " cmd)))))

(defn run-with-cofx [f]
  (let [cofx {}
        commands (f cofx)]
    (doseq [c commands]
      (execute-command c))))

(defn web-handler [r]
  {:headers {"Content-Type" "text/html; charset=utf-8"}
   :body (with-out-str (run-with-cofx app/main))})
