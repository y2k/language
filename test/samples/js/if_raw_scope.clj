;; true|true|true|true
(raw-code "let hostValue = 'outer'; let seen = ''; let tail = ''; let mutable = 'before';
function changeOuter() {mutable = 'after'; return true;}")
(def x "global")
(defn test []
  (let [local (let [x "local"] (raw-code "seen = x;") x)
        scope (str (let [hostValue "inner"] (raw-code "tail = hostValue;") hostValue)
                   (if true hostValue "unused"))
        changed (str (let [mutable "inner"] (changeOuter) mutable) (if true mutable "unused"))
        initializer (let [first x x (str x "-local")] (str first "|" x))]
    (str (and (= local "local") (= seen "local")) "|"
         (and (= scope "innerouter") (= tail "inner")) "|"
         (= changed "innerafter") "|" (= initializer "global|global-local"))))
