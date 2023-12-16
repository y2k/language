(defn start_hot_reload_client [request]
  (->
   (fetch
    "http://localhost:8787/"
    {:method "POST"
     :body request})
   (.then (fn [r] (.text r)))
   (.then (fn [cmd]
            (let [result (try (eval cmd) (catch e (str e)))]
              (.log console (str cmd " -> " result)))))
   (.catch (fn [e]
             (.error console e)
             (Promise. (fn [resolve] (setTimeout (fn [] (resolve "")) 5000)))))
   (.then (fn [cmd_result] (start_hot_reload_client cmd_result)))))

(start_hot_reload_client "")