;; true

(defn test []
  (let [seen (java.util.concurrent.atomic.AtomicBoolean. false)
        task (cast java.lang.Runnable
               (cond false ^void:java.lang.Runnable (fn [] (.set seen false))
                     :else ^void:java.lang.Runnable (fn [] (.set seen true))))]
    (.run task)
    (.get seen)))
