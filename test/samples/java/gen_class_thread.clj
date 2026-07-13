;; false

(gen-class
 :name GeneratedThread
 :extends java.lang.Thread
 :methods [[^override run [] void]])

(defn -run [this]
  nil)

(defn test []
  (let [t (GeneratedThread.)]
    (.interrupt t)
    (.isAlive t)))
