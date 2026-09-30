;; ok

(defn replaced [text pattern replacement expected]
  (let [actual (re-replace text (re-pattern pattern) replacement)]
    (if (= actual expected) "" (str " FAIL:" pattern ":" text ":" actual))))

(defn test []
  (str "ok"
    (replaced "foo1 foo22" "foo[0-9]+" "X" "X X")
    (replaced "abc" "x" "X" "abc")
    (replaced "export f" "^export " "" "f")
    (replaced "aa" "(a)" "$1\\1$&" "$1\\1$&$1\\1$&")
    (replaced "aa" "a" "aa" "aaaa")
    (replaced "aaa" "aa" "X" "Xa")
    (replaced "ab" "" "-" "-a-b-")
    (replaced "" "" "-" "-")
    (replaced "ab" "a*" "-" "-b-")
    (replaced "a" "a*" "-" "-")
    (replaced "ab" "^" "-" "-ab")
    (replaced "ab" "$" "-" "ab-")
    (replaced "ab" "^|$" "-" "-ab-")
    (replaced "ab" "a|$" "-" "-b-")
    (replaced "ab" "" "" "ab")
    (replaced "" "x" "-" "")
    (replaced "abc_42!!!" "[\\W]+" "-" "abc_42-")
    (replaced "abc_42!!!" "\\W+" "-" "abc_42-")
    (replaced "abc_42!!!" "[^\\w]+" "-" "abc_42-")
    (replaced "foo\n" "foo$" "X" "foo\n")
    (replaced "x$.$" "[$]" "-" "x-.-")
    (let [r (re-pattern "x")]
      (if (= (str (re-find r "x") "|" (re-find r "abc") "|"
                  (re-replace "xx" r "z") "|" (re-find r "xx"))
             "x|nil|zz|x")
        "" " FAIL:reuse"))))
