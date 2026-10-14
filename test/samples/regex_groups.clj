;; ok

(defn check [actual expected label]
  (if (= actual expected) "" (str " FAIL:" label)))

(defn groups [pattern text expected]
  (let [actual (re-find-groups (re-pattern pattern) text)]
    (str (check (count actual) (count expected) pattern)
      (reduce (fn [result index]
                (str result (check (get actual index) (get expected index) pattern)))
              "" [0 1 2 3]))))

(defn test []
  (let [r (re-pattern "post-([0-9]+)" "i")
        first (re-find-groups r "xPOST-5657 post-42")
        [whole id] ((fn [x] x) first)
        missing (re-find-groups r "absent")
        absent (get (re-find-groups (re-pattern "(a)?(b)") "b") 1)
        empty (get (re-find-groups (re-pattern "(a*)b") "b") 1)]
    (str "ok"
      (check whole "POST-5657" "whole") (check id "5657" "id")
      (check (get first 1) "5657" "get") (check (count first) 2 "count")
      (check (re-find r "POST-5657") "POST-5657" "re-find")
      (check missing nil "missing") (check (not= missing "nil") true "missing-type")
      (if missing " FAIL:missing-truth" "")
      (check absent nil "absent") (check (not= absent "nil") true "absent-type")
      (if absent " FAIL:absent-truth" "")
      (check empty "" "empty") (check (not= empty nil) true "empty-type")
      (if empty "" " FAIL:empty-truth")
      (groups "((ab)([0-9]+))" "xab42" ["ab42" "ab42" "ab" "42"])
      (groups "ab" "xab" ["ab"])
      (groups "(a)?(b)" "b" ["b" nil "b"])
      (groups "(a)|(b)" "b" ["b" nil "b"])
      (groups "(a)(b)?" "a" ["a" "a" nil])
      (groups "(a*)b" "b" ["b" ""])
      (groups "" "" [""]) (groups "" "abc" [""])
      (check (re-find (re-pattern "post") "POST") nil "default")
      (check (re-find (re-pattern "post" "") "POST") nil "empty-flags")
      (check (re-find (re-pattern "[a-z]+" "i") "ABC") "ABC" "class")
      (check (re-find (re-pattern "^foo" "i") "x\nFOO") nil "start")
      (check (re-find (re-pattern "foo$" "i") "FOO\n") nil "end")
      (check (re-find (re-pattern "a.b" "i") "A\nB") nil "dot")
      (check (re-replace "POST-1 post-2" r "$1") "$1 $1" "replace")
      (check (re-find r "POST-3") "POST-3" "reuse-find")
      (check (get (re-find-groups r "post-4") 1) "4" "reuse-groups")
      (check (re-replace "Ab" (re-pattern "a*" "i") "-") "-b-" "empty-replace"))))
