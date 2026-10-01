;; true|true
(raw-code "public static String hostTrace = \"T\";\npublic static int hostValue = 42;")

(defn test []
  (raw-code "hostTrace += \"A\";")
  (let [f (fn [x] (raw-code "hostTrace += \"L\";") x)]
    (raw-code "hostTrace += \"B\";")
    (f nil)
    (if false (do (raw-code "hostTrace += \"X\";") nil) nil)
    (if true (do (raw-code "hostTrace += \"C\";") nil) nil)
    (do (raw-code "hostTrace += \"D\";") nil)
    (let [] (raw-code "hostTrace += \"E\";") nil)
    (str (= hostTrace "TABLCDE") "|" (= hostValue 42))))
