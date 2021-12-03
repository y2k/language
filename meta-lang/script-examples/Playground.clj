(module
 (def state {})

 ;; ('a -> 'a) -> 'a
 (defn dispatch [f]
   (sys/intrinsic_set state model (f (:model state))))

  ;; () -> 'a
 (defn resolveEnv [])

  ;; Dictionary -> 'a
 (defn textView [props]
   (let [ctx (:ctx (resolveEnv))
         tv (sys/intrinsic_new android.widget.TextView ctx)]
     (sys/intrinsic_invoke android.widget.TextView/setText tv (:text props))))

  ;; int -> 'a
 (defn intToString [value]
   (sys/intrinsic_invoke_static java.lang.String/valueOf value))

  ;; Dictionary -> 'a
 (defn onClickHandle [model]
   {:count (add 1 (:count model))})

  ;; () -> 'a
 (defn onClick []
   (dispatch onClickHandle))

  ;; Dictionary -> 'a
 (defn view [model]
   (textView {:onClick onClick
              :text (intToString (:count model))})))

(module
 (def state {})
  ;; ('a -> 'b) -> 'c
 (defn dispatch [f]
   (sys/intrinsic_set state model (f (:model state))))
  ;; Unit -> 'a
 (defn resolveEnv [] ???)
  ;; Dictionary -> 'a
 (defn textView [props]
   (let [ctx (:ctx (resolveEnv))
         tv (sys/intrinsic_new android.widget.TextView ctx)]
     (sys/intrinsic_invoke android.widget.TextView/setText tv (:text props))))
  ;; int -> 'a
 (defn intToString [value]
   (sys/intrinsic_invoke_static java.lang.String/valueOf value))
  ;; Dictionary -> 'a
 (defn onClickHandle [model]
   {:count (add 1 (:count model))})
  ;; Unit -> 'a
 (defn onClick []
   (dispatch onClickHandle))
  ;; Dictionary -> 'a
 (defn view [model]
   (textView {:onClick onClick :text (intToString (:count model))})))