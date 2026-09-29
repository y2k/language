;; true

;; These known inputs have one initial runtime import and no "export " in JS strings.
(defn assemble-text [header-source runtime program]
  (let [header (re-find (re-pattern "// ==UserScript==[\\s\\S]*?// ==/UserScript==") header-source)
        exports (re-pattern "export ")
        body (re-replace program (re-pattern "^import [^\n]*\n") "")]
    (assert header)
    (str header "\n" (re-replace runtime exports "") "\n"
         (re-replace body exports ""))))

(defn assemble [header-path runtime-path program-path]
  (assemble-text (slurp header-path) (slurp runtime-path) (slurp program-path)))

(defn test []
  (= (assemble-text "before\n// ==UserScript==\n// @name demo\n// ==/UserScript==\nafter"
                    "export function helper() {}"
                    "import { helper } from './language_runtime.js';\nexport const answer = 42;")
     "// ==UserScript==\n// @name demo\n// ==/UserScript==\nfunction helper() {}\nconst answer = 42;"))
