;; true:true:true:false:false:true:true:false:true:1:done:2:3

(ns app.instance_check
  (:import [java.util ArrayList]))

(def counter (atom 0))

(defn make-value []
  (swap! counter (fn [n] (+ n 1)))
  "value")

(defn discarded []
  (instance? Object (make-value))
  "done")

(defn test []
  (let [checked (instance? String (let [value (make-value)] (if true value nil)))
        first-count (deref counter)
        result (discarded)
        second-count (deref counter)
        action ^void:java.lang.Runnable (fn [] (instance? Object (make-value)))]
    (.run action)
    (str (instance? ArrayList (ArrayList.)) ":"
         (instance? java.util.AbstractList (ArrayList.)) ":"
         (instance? java.util.List (ArrayList.)) ":"
         (instance? String nil) ":"
         (instance? Integer "hello") ":"
         (instance? Integer 42) ":"
         (instance? Boolean true) ":"
         (instance? String 42) ":"
         checked ":" first-count ":" result ":" second-count ":" (deref counter))))
