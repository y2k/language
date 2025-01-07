;; Declarations

(def - 0)
(def . 0)
(def * 0)
(def / 0)
(def % 0)
(def + 0)
(def < 0)
(def <= 0)
(def > 0)
(def >= 0)
(def as 0)
(def call-runtime 0)
(def class-inner 0)
(def gen-class-inner 0)
(def is 0)
(def new 0)
(def not 0)
(def set! 0)
(def unit 0)

;; Declarations Java

(def Object 0)
(def String 0)
(def int 0)
(def boolean 0)

(defmacro FIXME [& args]
  (list 'throw
        (list 'Exception.
              (concat
               (list
                'str
                (str "FIXME " __FILENAME__ ":" __LINE__ ":" (- __POSITION__ 1) " - "))
               args))))

;; Collections

(defmacro contains? [xs x] (list 'call-runtime ''contains xs x))
(defmacro assoc [xs k v] (list 'call-runtime ''assoc xs k v))
(defmacro concat [as bs] (list 'call-runtime ''concat as bs))
(defmacro conj [xs x] (list 'call-runtime ''conj xs x))
(defmacro count [xs] (list 'call-runtime ''count xs))
(defmacro empty? [xs] (list 'call-runtime ''empty xs))
(defmacro first [xs] (list 'get xs 0))
(defmacro list [& xs] (list 'java.util.LinkedList. (concat (list 'java.util.Arrays/asList) xs)))
(defmacro list? [x] (list 'is x "java.util.LinkedList"))
(defmacro map [f xs] (list 'call-runtime ''map f xs))
(defmacro map? [x] (list 'is x "java.util.Map"))
(defmacro merge [as bs] (list 'call-runtime ''merge as bs))
(defmacro reduce [f def xs] (list 'call-runtime ''reduce f def xs))
(defmacro rest [xs] (list 'call-runtime ''rest xs))
(defmacro run! [f xs] (list 'call-runtime ''run_ f xs))
(defmacro second [xs] (list 'get xs 1))
(defmacro vec [xs] (list 'java.util.Arrays/asList xs))
(defmacro vector? [x] (list 'is x "java.util.ArrayList"))

;; Other

(defmacro = [a b] (list 'call-runtime ''equals a b))
(defmacro def- [k v] (list 'def ^:private k v))
(defmacro get [target key] (list 'call-runtime ''get target key))
(defmacro get3 [target key default]
  (list 'let ['result (list 'get target key)]
        (list 'if (list '= 'nil 'result) default 'result)))

(defmacro into-array [xs] (list 'call-runtime ''into_array xs))
(defmacro into-array2 [type xs] (list 'call-runtime ''into_array type xs))
(defmacro println [& xs] (concat (list 'call-runtime ''println) xs))
(defmacro str [& xs] (concat (list 'call-runtime ''str) xs))
(defmacro throw [e] (list 'call-runtime ''throw_ e))
(defmacro unescape [x] (list 'call-runtime ''unescape x))

(defmacro atom [x] (list 'java.util.concurrent.atomic.AtomicReference. x))
(defmacro deref [a] (list '.get (list 'as a "java.util.concurrent.atomic.AtomicReference<Object>")))
(defmacro reset! [a x] (list '.set (list 'as a "java.util.concurrent.atomic.AtomicReference<Object>") x))

;; Java interop

(defmacro checked! [f] (list 'call-runtime ''try_ (list 'fn (vector) f)))
(defmacro class [cls] (list 'class-inner (list 'quote (symbol (str cls ".class")))))
(defmacro declare [name] (list 'def name))
(defmacro fn! [& body] (concat (list ^void 'fn) body))
(defmacro function [f] (list 'call-runtime ''function f))
(defmacro gen-class [& body] (list 'gen-class-inner (list 'quote body)))
(defmacro nil? [x] (list '= 'nil x))
(defmacro runnable [f] (list 'call-runtime ''runnable f))
(defmacro some? [x] (list 'not (list '= 'nil x)))
