(ns user
  (:import [com.sun.net.httpserver HttpServer HttpHandler HttpExchange]
           [java.util.concurrent CompletableFuture]))

(defonce commands (atom nil))

;; (defn handler [request]
;;   (loop []
;;     (if-let [cmd (first (swap-vals! commands (fn [x] nil)))]
;;       (if cmd
;;         cmd
;;         (do
;;           (Thread/sleep 500)
;;           (recur))))))

(defn handler [request]
  (loop [prev_cmd @commands
         cmd prev_cmd]
    (if (and cmd (not= prev_cmd cmd))
      cmd
      (do
        (Thread/sleep 500)
        (recur cmd @commands)))))

(comment

  (reset! commands "1 + 1")
  @commands

  (.stop server 1)

  (def server
    (let [server (HttpServer/create (java.net.InetSocketAddress. 8787) 0)
          handler (reify HttpHandler
                    (handle [_ exchange]
                      (println "HANDLE")
                      (let [request-body (slurp (.getRequestBody exchange))
                            response (.getBytes (handler request-body))]
                        (.set (.getResponseHeaders exchange) "Access-Control-Allow-Origin" "*")
                        (.set (.getResponseHeaders exchange) "Access-Control-Allow-Headers" "Origin, X-Requested-With, Content-Type, Accept")
                        (.sendResponseHeaders exchange 200 (count response))
                        (let [os (.getResponseBody exchange)]
                          (.write os response)
                          (.close os)))))]
      (.createContext server "/" handler)
      (.setExecutor server nil)
      (.start server)
      server))

  comment)
