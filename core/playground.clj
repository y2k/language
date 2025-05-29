(do*
 (def* main_ui
   (fn* ()
        (java.util.Arrays.asList
         :row (java.util.Map.of)
         (java.util.Arrays.asList
          :button (java.util.Map.of :title "QR" :onclick (G31android_gallery.android_gallery9get_image)))
         (java.util.Arrays.asList
          :button (java.util.Map.of :title "Settings" :onclick (G15chat_ui.chat_ui9update_ui (java.util.Arrays.asList :button (java.util.Map.of :title "TEST 2" :onclick nil))))))))
 (def* main (fn* () (G15chat_ui.chat_ui9update_ui (main_ui))))
 (def- w_atom (atom (java.util.Map.of)))
 (def* execute_fx (fn* (fx) (fx (deref w_atom))))
 (def* main_event
   (fn* (uri)
        (G21android_qr.android_qr9decode_qr
         uri
         (java.util.Map.of
          :callback
          (fn* (p__1)
               (do*
                (let* p__2 p__1)
                (let* p__3 0)
                (let* p__6)
                (if*
                 (instance? java.util.Map p__2)
                 (set! p__6 (. (cast java.util.Map p__2) get p__3))
                 (set! p__6 (. (cast java.util.List p__2) get (cast int p__3))))
                (let* p__4 p__1)
                (let* p__5 1)
                (let* p__7)
                (if*
                 (instance? java.util.Map p__4)
                 (set! p__7 (. (cast java.util.Map p__4) get p__5))
                 (set! p__7 (. (cast java.util.List p__4) get (cast int p__5))))
                (let* (vector p__6 x p__7)
                      (execute_fx
                       (G15chat_ui.chat_ui9update_ui
                        (java.util.Arrays.asList
                         :label
                         (java.util.Map.of :text (String.format "%s" x))))))))))))

 (def* activity_onCreate (fn* (^MainActivity self ^Bundle bundle) (do* (let* root (G15chat_ui.chat_ui5root_ self)) (. self setContentView root) (G15chat_ui.chat_ui19add_effect_handlers self root w_atom) (swap! w_atom (fn* (w) (G31android_gallery.android_gallery21attach_effect_handler self w))) (swap! w_atom (fn* (w) (G21android_qr.android_qr21attach_effect_handler self w))) (swap! w_atom (fn* (w) (G21android_db.android_db21attach_effect_handler (java.util.Map.of :db ":memory:") w))) (execute_fx (main)))))
 (def* activity_onActivityResult (fn* (^MainActivity self ^int requestCode ^int resultCode ^Intent data) (do* (let* uri (G31android_gallery.android_gallery18on_activity_result self requestCode resultCode data)) (execute_fx (main_event uri)))))

 (__compiler_emit "
public static class MainActivity extends Activity {
  public void onCreate(Bundle p0) {
    return (void)y2k.RT.invoke(activity_onCreate,this,p0);
  }
  public void onActivityResult(int p0,int p1,Intent p2) {
    return (void)y2k.RT.invoke(activity_onActivityResult,this,p0,p1,p2);
  }
}"))

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
