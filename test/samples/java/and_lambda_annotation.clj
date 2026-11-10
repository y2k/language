;; true

(defn test []
  (let [seen (java.util.concurrent.atomic.AtomicBoolean. false)
        task (cast java.lang.Runnable
               (and true ^void:java.lang.Runnable (fn [] (.set seen true))))]
    (.run task)
    (.get seen)))
