(ns research-framework
  (:require [app :as app]))

(comment

  (run-with-cofx app/main)
  @db

  ())

(def db (atom {}))

(defn download-http [url]
  (->
   (java.net.http.HttpClient/newHttpClient)
   (.send (->
           (java.net.http.HttpRequest/newBuilder)
           (.uri (java.net.URI/create url))
           (.build))
          (java.net.http.HttpResponse$BodyHandlers/ofString (java.nio.charset.Charset/forName "UTF-8")))
   (.body)))

(declare execute-command)

(defn run-with-cofx-2 [f arg]
  (let [cofx {}
        commands (f cofx arg)]
    (doseq [c commands]
      (execute-command c))))

;; callback

(defn execute-command [[cmd arg]]
  (case cmd
    :download-http
    (let [page (download-http (:url arg))
          callback (:callback arg)]
      (run-with-cofx-2 callback page))
    :db (reset! db arg)
    :parse-html (throw (Exception. ":parse-html not implemented"))))

(defn run-with-cofx [f]
  (let [cofx {}
        commands (f cofx)]
    (doseq [c commands]
      (execute-command c))))
