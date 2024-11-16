(ns im.y2k.chargetimer
  (:import [android.app Activity NotificationChannel Notification NotificationManager]
           [android.webkit WebView JavascriptInterface]
           [android.content Intent Context IntentFilter BroadcastReceiver ComponentName]
           [android.media AudioManager RingtoneManager]
           [android.app.job JobScheduler JobParameters JobInfo]
           [java.util List Objects]
           [java.util.function BiFunction]))

(gen-class
 :name WebViewJsListener
 :extends Object
 :constructors {[Activity WebView] []}
 :prefix "wv_"
 :methods [[^JavascriptInterface dispatch [String String] void]])

(defn- wv_dispatch [^WebViewJsListener self ^String event ^String payload]
  (let [activity (as (get self.state 0) Activity)
        wv (get self.state 1)]
    (.runOnUiThread! activity (fn! [] (InterpreterKt/run_code activity wv event)))))

(defn main [^Activity context ^WebView webview]
  (let [webSettings (.getSettings webview)]
    (.setJavaScriptEnabled webSettings true)
    (.setAllowUniversalAccessFromFileURLs webSettings true)
    (.addJavascriptInterface webview (WebViewJsListener. context webview) "Android")
    (.loadUrl! webview "file:///android_asset/index.html")))

(gen-class
 :name ChargeJobService
 :extends android.app.job.JobService
 :constructors {[] []}
 :prefix "cj_"
 :methods [[^Override onStartJob [JobParameters] boolean]
           [^Override onStopJob [JobParameters] boolean]])

(defn cj_onStartJob [^ChargeJobService self ^JobParameters p]
  (InterpreterKt/run_code self null :job_scheduled)
  false)

(defn cj_onStopJob [^ChargeJobService self ^JobParameters p]
  false)

;; (defn play_alarm [^Context context]
;;   (let [am (as (.getSystemService context Context/AUDIO_SERVICE) AudioManager)
;;         sound_stream_id 5
;;         max (.getStreamMaxVolume am sound_stream_id)]
;;     (.setStreamVolume am sound_stream_id max 0)
;;     (let [notification (.getDefaultUri RingtoneManager RingtoneManager/TYPE_ALARM)
;;           r (.getRingtone RingtoneManager context notification)]
;;       (.play r))))

(defn ^"BiFunction<String,Object,Object>" make_dispatch [^Activity activity ^WebView webView]
  (fn [^String event ^Object payload]
    (case event

      :start_job
      (let [job_info (->
                      (JobInfo.Builder. 123 (ComponentName. activity "im.y2k.chargetimer.ChargeJobService"))
                      (.setPeriodic 300000)
                      (.setRequiresCharging true)
                      .build)
            job_scheduler (as (.getSystemService activity Context.JOB_SCHEDULER_SERVICE) JobScheduler)]
        (.schedule job_scheduler job_info))

      :stop_job
      (.cancel!
       (.getSystemService activity (class JobScheduler))
       123)

      :get_job_info
      (let [callback "(FIXME)"
            service (as (.getSystemService activity Context.JOB_SCHEDULER_SERVICE) JobScheduler)
            m (android.app.job.JobInfo/getMinPeriodMillis)
            reason (.getPendingJob service 123)]
        (.evaluateJavascript! webView (str callback "('" m " / " reason "')") null))

      null)))

;; (gen-class
;;  :name BatteryBroadcastReceiver
;;  :extends BroadcastReceiver
;;  :constructors {["(String)->Unit"] []}
;;  :prefix "br_"
;;  :methods [[^Override onReceive [Context Intent] Unit]])

;; (defn- br_onReceive [^BatteryBroadcastReceiver self ^Context context ^Intent intent]
;;   (let [extras (requireNotNull intent.extras)
;;         callback (as (get self.state 0) "(String)->Unit")
;;         allValues (->
;;                    extras
;;                    .keySet
;;                    (.map (fn [key] (Pair. key (.get extras key))))
;;                    (.associate (fn [x] x)))]
;;     (callback (.toJson (com.google.gson.Gson.) allValues))))

;; (defn do_register_receiver [^Context context ^String action ^"(String)->Unit" callback]
;;   (.registerReceiver
;;    context
;;    (BatteryBroadcastReceiver. callback)
;;    (IntentFilter. action)))
