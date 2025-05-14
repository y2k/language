;;

(defn run (vector)
  (count (vector 10 20 30)))

(def* run
  (fn* ()
       (do*
        (let* p__1 (vector 10 20 30))
        (if
         (instance? java.util.Map p__1)
          (. (cast java.util.Map p__1) size)
          (. (cast java.util.Collection p__1) size)))))

;;

(ns _
  (:import [java.util Date]))

(defn run []
  (.hashCode (Date. 2)))

;; ->

(ns _
  (:import [java.util Date]))

(defn run []
  (.hashCode (new Date 2)))

;; ->

(defn run []
  (.hashCode (java.util.Date. 2)))

;;

(defn list [& xs] xs)

(defn macro_get [xs i]
  (list '. (list 'cast 'java.util.List xs) 'get (list 'cast 'int i)))

(defn macro_str [& xs]
  (concat
   (list
    (quote* String.format)
    (reduce (fn* [acc x] (str acc "%s")) "" xs))
   xs))

(defn macro_vector [& xs]
  (concat
   (list (quote* java.util.Arrays.asList))
   xs))

;;

(if (instance? java.util.Map (hash-map :a 1 :b 2))
  (. (cast java.util.Map (hash-map :a 1 :b 2)) size)
  (. (cast java.util.Collection (hash-map :a 1 :b 2)) size))

;;
(if  (instance? java.util.Map (vector 10 20 30)) (. (cast java.util.Map (vector 10 20 30)) size) (. (cast java.util.Collection (vector 10 20 30)) size))
(if* (instance? java.util.Map (vector 10 20 30)) (. (cast java.util.Map (vector 10 20 30)) size) (. (cast java.util.Collection (vector 10 20 30)) size))
;;

(def* run
  (fn* ()
       (do*
        (let* p__1)
        (let* p__2)
        (if*
         (instance? java.util.Map (vector 10 20 30))
         (set! p__2 (set! p__1 (. (cast java.util.Map (vector 10 20 30)) size)))
         (set! p__2 (set! p__1 (. (cast java.util.Collection (vector 10 20 30)) size))))
        p__1)))

;;
