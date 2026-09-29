;; foo42|true true|true true|X X|-a-b-|-b-|$1\1$1\1|a

(defn test []
  (let [r (re-pattern "foo([0-9]+)")
        missing (re-find r "none")
        empty (re-find (re-pattern "") "abc")]
    (str (re-find r "xfoo42 foo7") "|"
         (= missing nil) " " (not missing) "|"
         (= empty "") " " (if empty true false) "|"
         (re-replace "foo1 foo22" r "X") "|"
         (re-replace "ab" (re-pattern "") "-") "|"
         (re-replace "ab" (re-pattern "a*") "-") "|"
         (re-replace "aa" (re-pattern "(a)") "$1\\1") "|"
         (re-find (re-pattern "a+?") "aaa"))))
