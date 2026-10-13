;; true true true true true true true true

(ns checks.collections.core)

(defn test []
  (let [empty []
        one (conj empty 1)
        source [1 2]
        appended (conj source 3)
        listed (conj (list 1 2) 3)
        values (reduce (fn [acc x] (conj acc x)) [] [nil false 0 "" [1 2]])
        original (atom 10)
        added (atom 20)
        refs (conj [original] added)]
    (reset! (get refs 0) 11)
    (reset! (get refs 1) 21)
    (str
     (and (= (count empty) 0) (= (count one) 1) (= (get one 0) 1)) " "
     (and (= (count source) 2) (= (get source 0) 1) (= (get source 1) 2)
          (= (count appended) 3) (= (get appended 0) 1) (= (get appended 1) 2) (= (get appended 2) 3)) " "
     (and (= (count listed) 3) (= (get listed 0) 1) (= (get listed 1) 2) (= (get listed 2) 3)) " "
     (and (= (count values) 5) (= (get values 0) nil) (not= (get values 0) "nil") (not (get values 0))) " "
     (and (= (get values 1) false) (not= (get values 1) "false") (not (get values 1))) " "
     (and (= (get values 2) 0) (= (get values 3) "")) " "
     (and (vector? (get values 4)) (= (count (get values 4)) 2)
          (= (get-in values [4 0]) 1) (= (get-in values [4 1]) 2)) " "
     (and (= (deref original) 11) (= (deref added) 21)))))
