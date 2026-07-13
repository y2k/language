;; true

(defn test []
  (let [seen (java.util.concurrent.atomic.AtomicBoolean. false)
        task ^void:java.lang.Runnable
        (fn []
          (.set seen true))]
    (.run task)
    (.get seen)))
