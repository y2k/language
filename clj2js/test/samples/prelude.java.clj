(defmacro not= [a b] (list 'not (list '= a b)))

(defmacro gen-class [& body] (list '__inject_raw_sexp (concat (list 'gen-class) body)))

(defmacro fn! [& body] (concat (list ^void 'fn) body))

(defmacro str [& xs] (concat (list 'y2k.RT/str) xs))

(defmacro checked! [f] (list 'y2k.RT/try_ (list 'fn (vector) f)))

(defmacro get [target key] (list 'y2k.RT/get target key))

(defmacro println [& xs] (list 'do (list 'System.out/println (concat (list 'str) xs)) 'null))

(defmacro js! [& body] (list 'comment body))

(defmacro jvm! [& body] (concat (list 'module) body))
