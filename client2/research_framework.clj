(ns research-framework
  (:require [app :as app]))

(comment

  (update {:a {:b 1}} :a (fn [x] (assoc x :b 2)))

  ())

(def db (atom {}))

(defn execute-command [[cmd arg]]
  (case cmd
    :db (throw (Exception. "not implemented"))
    :download-http (throw (Exception. "not implemented"))
    :parse-html (throw (Exception. "not implemented"))))

(defn run-with-cofx [f]
  (let [cofx {}
        commands (f cofx)]
    (doseq [c commands]
      (execute-command c))))
