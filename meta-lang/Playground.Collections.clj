(module
 ;; 'a -> 'a -> bool
 ;; (defn is-same [a b] ???)
 ;; (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
 ;; (defn fix [f start-value] ???)
 ;; ('a -> 'b -> 'c -> 'd) -> 'a -> ('b -> 'c -> 'd)
 ;; (defn apply3 [f first-arg] ???)

 (def list-nil dic-nil)

 (defn if [condition then? else?] ???)

 (def render-item
   (fn [x]
     (ui-text (dic-add dic-nil "text" x))))

 (defn list-map' [f list-map' xs]
   (if
    (is-same list-nil xs)
     list-nil
     (list-cons
      (dic-add xs "value" (f (dic-get xs "value")))
      (list-map' (dic-get xs "next") f))))

 (defn list-map [xs f]
   (fix
    (apply3 list-map' f)
    xs))

 (defn list-map [xs f]
   (if
    (is-same list-nil xs)
     list-nil
     (list-cons
      (dic-add xs "value" (f (dic-get xs "value")))
      (list-map (dic-get xs "next") f)))))
