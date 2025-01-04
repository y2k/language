let js =
  {|
;; JS prelude
;; (def quote 0)
(def __raw_template 0)
(def . 0)
(def assoc 0)
(def catch 0)
(def comment 0)
(def do 0)
(def false 0)
(def fn* 0)
(def hash-map 0)
(def let* 0)
(def new 0)
(def ns 0)
(def null 0)
(def set 0)
(def set! 0)
(def spread 0)
(def true 0)
(def try 0)
(def vector 0)
(def while 0)

(defmacro FIXME [& args]
  (list 'throw
        (list 'Error.
              (concat
               (list
                'str
                (str "FIXME " __FILENAME__ ":" __LINE__ ":" (- __POSITION__ 1) " - "))
               args))))

(defmacro def- [k v] (list 'def ^:private k v))
(defmacro do [& body] (concat (list 'let (vector)) body))
(defmacro eprintln [& args] (concat (list '.error 'console) args))
(defmacro first [xs] (list 'get (list '.from 'Array xs) 0))
(defmacro js! [& body] (concat (list 'do) body))
(defmacro jvm! [& body] (list 'comment body))
(defmacro not= [a b] (list 'not (list '= a b)))
(defmacro println [& args] (concat (list '.info 'console) args))
(defmacro rest [xs] (list '.toSpliced (list '.from 'Array xs) 0 1))
(defmacro second [xs] (list 'get (list '.from 'Array xs) 1))
(defmacro str [& args] (concat (list '+ "") args))
(defmacro some? [x] (list 'not (list 'nil? x)))
(defmacro string? [x] (list '= :string (list 'type x)))
(defmacro number? [x] (list '= :number (list 'type x)))
(defmacro seq? [x] (list 'Array.isArray x))
(defmacro map? [x] (list '__raw_template "(" x " instanceof Map)"))
;; (defmacro reduce [xs f init] (list '.reduce xs f init))
(defmacro reduce [xs f init]
  (list '__raw_template
        "(function() {
          const xs=" xs ";
          const f=" f ";
          const init=" init ";
          if (Array.isArray(xs)){return xs.reduce(f,init)}
          else{return Object.entries(xs).reduce(f,init)}})()"))

(defmacro atom [x] (vector x))
(defmacro deref [x] (list 'get x 0))
(defmacro reset! [a x] (list 'do (list '.fill a x) x))
(defmacro swap! [a f] (list 'get (list '.splice a 0 1 (list f (list 'get a 0))) 0))

(defmacro empty? [xs] (list '= 0 (list '.-length xs)))

(def concat 0)
(def list 0)
(def transform_nodes 0)

(defn- make_operator [sep xs]
  (concat
   (list '__raw_template "(")
   (transform_nodes {:sep sep} xs)
   (list ")")))

(defmacro - [& xs] (make_operator " - " xs))
(defmacro * [& xs] (make_operator " * " xs))
(defmacro + [& xs] (make_operator " + " xs))
(defmacro and [& xs] (make_operator " && " xs))
(defmacro or [& xs] (make_operator " || " xs))

(defmacro / [a b] (list '__raw_template "(" a " / " b ")"))
(defmacro % [a b] (list '__raw_template "(" a " % " b ")"))
(defmacro < [a b] (list '__raw_template "(" a " < " b ")"))
(defmacro <= [a b] (list '__raw_template "(" a " <= " b ")"))
(defmacro = [a b] (list '__raw_template "" a " === " b))
(defmacro > [a b] (list '__raw_template "(" a " > " b ")"))
(defmacro >= [a b] (list '__raw_template "(" a " >= " b ")"))
(defmacro assoc! [col key value] (list '__raw_template "" col "[" key "]=" value))
(defmacro concat [a b] (list '__raw_template "[..." a ", ..." b "]"))
(defmacro conj [a b] (list '__raw_template "[..." a ", " b "]"))
(defmacro cons [a b] (list '__raw_template "[" a ", ..." b "]"))
(defmacro export-default [body] (list '__raw_template "export default " body))
(defmacro get [target index] (list '__raw_template "" target "[" index "]"))
;; (defmacro if [c a b] (list '__raw_template "(" c " ? " a " : " b ")"))
(defmacro merge [a b] (list '__raw_template "{ ..." a ", ..." b " }"))
(defmacro nil? [x] (list 'or (list '= 'null x) (list '= 'undefined x)))
(defmacro not [x] (list '__raw_template "!(" x ")"))
;; (defmacro set! [target value] (list '__raw_template "(" target " = " value ");"))
;; (defmacro spread [a] (list '__raw_template "..." a))
(defmacro throw [ex] (list '__raw_template "(function(){throw " ex "})()"))
(defmacro type [x] (list '__raw_template "typeof " x))

;; (defmacro quote [n]
;;   {:__y2k_type :quote
;;    :value (str n)})

(defmacro quote_of_string [n]
  {:__y2k_type :quote
   :value n})

(defmacro quote? [x]
  (list '= :quote (list '.-__y2k_type x)))

;; (defmacro vector [& args]
;;   (concat
;;    (list '__raw_template "[")
;;    (transform_nodes {:sep ","} args)
;;    (list "]")))

(defmacro list [& args]
  (list 'let ['xs (concat (list 'vector) args)]
        (list 'set! (list '.-__y2k_type 'xs) :list)
        'xs))

(defmacro list? [x]
  (list '= :list (list '.-__y2k_type x)))

;; HTML
(def alert 0)
(def Array 0)
(def BarcodeDetector 0)
(def Buffer 0)
(def console 0)
(def createImageBitmap 0)
(def crypto 0)
(def Date 0)
(def debugger 0)
(def document 0)
(def Error 0)
(def eval 0)
(def fetch 0)
(def globalThis 0)
(def JSON 0)
(def localStorage 0)
(def Math 0)
(def Object 0)
(def parseInt 0)
(def process 0)
(def Promise 0)
(def RegExp 0)
(def Response 0)
(def setTimeout 0)
(def undefined 0)
(def window 0)

;; Debug

(defmacro DEBUG [message value]
  (list 'do
        (list 'eprintln message value)
        value))
|}

let java =
  {|
;; Common

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
(defmacro do [& xs] (concat (list 'let (vector)) xs))
(defmacro get [target key] (list 'call-runtime ''get target key))
(defmacro get3 [target key default]
  (list 'let ['result (list 'get target key)]
        (list 'if (list '= 'null 'result) default 'result)))

(defmacro into-array [xs] (list 'call-runtime ''into_array xs))
(defmacro into-array2 [type xs] (list 'call-runtime ''into_array type xs))
(defmacro js! [& body] (list 'comment body))
(defmacro jvm! [& body] (concat (list 'do) body))
(defmacro not= [a b] (list 'not (list '= a b)))
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
(defmacro nil? [x] (list '= 'null x))
(defmacro runnable [f] (list 'call-runtime ''runnable f))
(defmacro some? [x] (list 'not (list '= 'null x)))

;; Declarations

(def - 0)
(def . 0)
(def * 0)
(def / 0)
(def % 0)
(def + 0)
(def < 0)
(def <= 0)
(def = 0)
(def > 0)
(def >= 0)
(def as 0)
(def call-runtime 0)
(def catch 0)
(def class-inner 0)
(def do 0)
(def false 0)
(def fn* 0)
(def gen-class-inner 0)
(def gen-class* 0)
(def get 0)
(def hash-map 0)
(def if 0)
(def is 0)
(def let* 0)
(def new 0)
(def not 0)
(def ns 0)
(def null 0)
(def quote 0)
(def set! 0)
(def throw 0)
(def true 0)
(def try 0)
(def unit 0)
(def vector 0)
(def while 0)

;; Declarations Java

(def Object 0)
(def String 0)
(def int 0)
(def boolean 0)
|}

let interpreter =
  {|
(def __FILENAME__ 0)
(def __LINE__ 0)
(def __POSITION__ 0)
(def - 0)
(def * 0)
(def / 0)
(def + 0)
(def = 0)
(def concat 0)
(def count 0)
(def drop 0)
(def false 0)
(def FIXME 0)
(def get 0)
(def hash-map 0)
(def if 0)
(def list 0)
(def map 0)
(def map? 0)
(def null 0)
(def println 0)
(def reduce 0)
(def some? 0)
(def str 0)
(def true 0)
(def vec 0)
(def vector 0)
(def vector? 0)

(defmacro not= [a b] (list 'not (list '= a b)))

(defn not [x] (if x false true))
|}

let bytecode =
  {|
(def - 0)
(def * 0)
(def / 0)
(def + 0)
(def = 0)
(def atom 0)
(def concat 0)
(def deref 0)
(def false 0)
(def get 0)
(def hash-map 0)
(def if 0)
(def list 0)
(def ns 0)
(def null 0)
(def println 0)
(def reset! 0)
(def str 0)
(def true 0)
(def vec 0)
(def vector 0)

(defmacro defonce [name value]
  (list 'if
        (list 'def name)
        'nil
        (list 'def name value)))

(defmacro not= [a b] (list 'not (list '= a b)))

(defn not [x] (if x false true))
|}
