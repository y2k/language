;; [:play_alarm
;; (fn [env]
;;   (let [context (:context env)
;;         am (.getSystemService context (.loadClass (.getClassLoader (.getClass "")) "android.media.AudioManager"))
;;         sound_stream_id 5
;;         max (.getStreamMaxVolume am sound_stream_id)]
;;     (.setStreamVolume am sound_stream_id max 0)
;;     (let [notification (.getDefaultUri android.media.RingtoneManager 4)
;;           r (.getRingtone android.media.RingtoneManager context notification)]
;;       (.play r))))]

(defmacro str [& xs]
  (list '.join 'String "" (vec xs)))

(defn start_job [env]
  (let [wv (:webview env)
        context (:context env)
        r (.registerReceiver context null (android.content.IntentFilter. "android.intent.action.BATTERY_CHANGED"))
        level (.getIntExtra r "level" -1)]
    (.evaluateJavascript wv (str "window.update_ui(\"#text_job_status\", " (.toString level) ")") null)))

(defn job_scheduled [env]
  (let [context (:context env)
        result (.registerReceiver context null (android.content.IntentFilter. "android.intent.action.BATTERY_CHANGED"))
        level (.getIntExtra result "level" -1)]
    (if (> level 90)
      (play_alarm context)
      null)))

;; [[:start_job start_job] [:job_scheduled job_scheduled]]
