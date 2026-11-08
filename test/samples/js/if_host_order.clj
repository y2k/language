;; true|true|true|true|true
(raw-code "let hostTrace = ''; let hostValue = 'old';
const hostFailure = new Error('stop');
function early() { hostTrace += 'early'; throw hostFailure; }
function condition() { hostTrace += 'condition'; return true; }
function makeText() { hostTrace += 'receiver'; return new String('a'); }
function changeValue() { hostValue = 'new'; return true; }
function failureCheck() { try { probe(); return false; } catch (e) { return e === hostFailure; } }")
(defn probe [] (str (early) (if (condition) "selected" "unused") (makeText)))
(defn test []
  (let [failed (failureCheck)
        stopped (= hostTrace "early")]
    (raw-code "hostTrace = '';")
    (let [value (. (let [x (makeText)] x) concat (str (if (condition) "b" "unused")))
          receiver (and (= value "ab") (= hostTrace "receivercondition"))
          before (str hostValue (if (changeValue) hostValue "unused"))
          f (fn [x] (str "original" x))
          called (f (if true (do (set! f (fn [x] (str "replaced" x))) "!")))]
      (str failed "|" stopped "|" receiver "|" (= before "oldnew") "|" (= called "original!")))))
