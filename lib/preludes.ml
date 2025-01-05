(* THIS FILE IS GENERATED *)

let java_runtime = {|
package y2k;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

@SuppressWarnings("unchecked")
public class RT {

  public static Object run_(Object farg, Object xs) {
    var f = (Function<Object, Object>) farg;
    var col = (List<Object>) xs;
    for (Object x : col) {
      f.apply(x);
    }
    return null;
  }

  public static Object reduce(Object f, Object def, Object xs) {
    var col = (Collection<Object>) xs;
    var func = (Fn<Object, Object>) f;
    var acc = def;
    for (Object x : col) {
      acc = func.invoke(acc, x);
    }
    return acc;
  }

  public static boolean contains(Object xs, Object x) {
    if (xs instanceof Map) {
      var map = (Map<Object, Object>) xs;
      return map.containsKey(x);
    }
    if (xs instanceof Collection) {
      var col = (Collection<Object>) xs;
      return col.contains(x);
    }
    throw new RuntimeException("Unsupported source: " + xs);
  }

  public static List<Object> map(Object f, Object xs) {
    var col = (Collection<Object>) xs;
    var result = new ArrayList<Object>(col.size());
    var func = (Function<Object, Object>) f;
    for (Object x : col) {
      result.add(func.apply(x));
    }
    return result;
  }

  public static List<Object> rest(Object xs) {
    var col = (List<Object>) xs;
    return col.subList(1, col.size());
  }

  public static boolean equals(Object a, Object b) {
    return java.util.Objects.equals(a, b);
  }

  public static Map<Object, Object> hash_map(Object... args) {
    var result = new HashMap<Object, Object>();
    for (int i = 0; i < args.length; i += 2) {
      result.put(args[i], args[i + 1]);
    }
    return result;
  }

  public static int count(Object xs) {
    return ((List<Object>) xs).size();
  }

  public static <T, R> Function<T, R> function(Function<T, R> f) {
    return f;
  }

  public static Map<Object, Object> merge(Object as, Object bs) {
    var a = (Map<Object, Object>) as;
    var b = (Map<Object, Object>) bs;
    var result = new HashMap<>(a);
    result.putAll(b);
    return result;
  }

  public static List<Object> concat(Object as, Object bs) {
    var a = (List<Object>) as;
    var b = (List<Object>) bs;
    var result = new ArrayList<>(a);
    result.addAll(b);
    return result;
  }

  public static Map<Object, Object> assoc(Object xs, Object k, Object v) {
    var col = (Map<Object, Object>) xs;
    var result = new HashMap<>(col);
    result.put(k, v);
    return result;
  }

  public static boolean empty(Object xs) {
    return ((List<Object>) xs).isEmpty();
  }

  public static List<Object> conj(Object xs, Object x) {
    var col = (Collection<Object>) xs;
    var result = new ArrayList<>(col);
    result.add(x);
    return result;
  }

  public static Object[] into_array(Object xs) {
    var col = (List<Object>) xs;
    var result = (Object[]) Array.newInstance(col.get(0).getClass(), col.size());
    return col.toArray(result);
  }

  public static Object[] into_array(Class<?> cls, Object xs) {
    var col = (List<Object>) xs;
    var result = (Object[]) Array.newInstance(cls, col.size());
    return col.toArray(result);
  }

  public static String str(Object... args) {
    if (args.length == 1) {
      return Objects.toString(args[0]);
    }
    StringBuilder sb = new StringBuilder();
    for (Object arg : args) {
      sb.append(arg);
    }
    return sb.toString();
  }

  public static <T> T get(Object source, Object key) {
    if (source instanceof java.util.Map) {
      return (T) ((java.util.Map<?, ?>) source).get(key);
    }
    if (source instanceof java.util.List) {
      return (T) ((java.util.List<?>) source).get((Integer) key);
    }
    throw new RuntimeException("Unsupported source: " + source + ", key: " + key);
  }

  private static <T extends Throwable> void throwException(
      Throwable exception,
      Object dummy) throws T {
    throw (T) exception;
  }

  public static <T> T throw_(Object exception) {
    throwException((Throwable) exception, null);
    return null;
  }

  public static <T> T try_(Object f) {
    try {
      return (T) ((Fn) f).invoke();
    } catch (Exception e) {
      RT.throwException(e, null);
      return null;
    }
  }

  public static Object println(Object... xs) {
    for (Object x : xs) {
      System.out.print(x);
      System.out.print(" ");
    }
    System.out.println();
    return null;
  }

  public static Runnable runnable(Supplier<Object> f) {
    return f::get;
  }

  public static String unescape(Object input) {
    var str = input.toString();
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < str.length(); i++) {
      char c = str.charAt(i);
      if (c == '\\' && i + 1 < str.length()) {
        char next = str.charAt(i + 1);
        if (next == 'n') {
          sb.append('\n');
          i++;
        } else if (next == '\\') {
          sb.append('\\');
          i++;
        } else if (next == '\"') {
          sb.append('\"');
          i++;
        } else {
          sb.append(c);
        }
      } else {
        sb.append(c);
      }
    }
    return sb.toString();
  }

  public static Object invoke(Object f, Object... args) {
    var fn = (Fn) f;
    if (args.length == 0)
      return fn.invoke();
    if (args.length == 1)
      return fn.invoke(args[0]);
    if (args.length == 2)
      return fn.invoke(args[0], args[1]);
    if (args.length == 3)
      return fn.invoke(args[0], args[1], args[2]);
    if (args.length == 4)
      return fn.invoke(args[0], args[1], args[2], args[3]);
    throw new RuntimeException("Unsupported arity: " + args.length);
  }

  public interface Fn0 {
    Object invoke() throws Exception;
  }

  public interface Fn1 {
    Object invoke(Object a1) throws Exception;
  }

  public interface Fn2 {
    Object invoke(Object a1, Object a2) throws Exception;
  }

  public interface Fn3 {
    Object invoke(Object a1, Object a2, Object a3) throws Exception;
  }

  public interface Fn4 {
    Object invoke(Object a1, Object a2, Object a3, Object a4) throws Exception;
  }

  public static class Fn<TI, TR> implements Runnable, Callable<TR>, Consumer<TI>, Function<TI, TR>,
      Supplier<TR>, Fn0, Fn1, Fn2, Fn3, Fn4 {
    public Object invoke() {
      return invoke(null);
    }

    public Object invoke(Object a1) {
      return invoke(a1, null);
    }

    public Object invoke(Object a1, Object a2) {
      return invoke(a1, a2, null);
    }

    public Object invoke(Object a1, Object a2, Object a3) {
      return invoke(a1, a2, a3, null);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4) {
      throw new RuntimeException("Not implemented");
    }

    public final void run() {
      invoke();
    }

    public final TR call() {
      return (TR) invoke();
    }

    @Override
    public final void accept(TI t) {
      invoke(t);
    }

    @Override
    public final TR apply(TI t) {
      return (TR) invoke(t);
    }

    @Override
    public final TR get() {
      return (TR) invoke();
    }
  }

  public static Fn fn(Fn0 f) {
    return new Fn() {
      @Override
      public Object invoke() {
        try {
          return f.invoke();
        } catch (Exception e) {
          return throw_(e);
        }
      }
    };
  }

  public static Fn fn(Fn1 f) {
    return new Fn() {
      @Override
      public Object invoke(Object a1) {
        try {
          return f.invoke(a1);
        } catch (Exception e) {
          return throw_(e);
        }
      }
    };
  }

  public static Fn fn(Fn2 f) {
    return new Fn() {
      @Override
      public Object invoke(Object a1, Object a2) {
        try {
          return f.invoke(a1, a2);
        } catch (Exception e) {
          return throw_(e);
        }
      }
    };
  }

  public static Fn fn(Fn3 f) {
    return new Fn() {
      @Override
      public Object invoke(Object a1, Object a2, Object a3) {
        try {
          return f.invoke(a1, a2, a3);
        } catch (Exception e) {
          return throw_(e);
        }
      }
    };
  }

  public static Fn fn(Fn4 f) {
    return new Fn() {
      @Override
      public Object invoke(Object a1, Object a2, Object a3, Object a4) {
        try {
          return f.invoke(a1, a2, a3, a4);
        } catch (Exception e) {
          return throw_(e);
        }
      }
    };
  }
}
|}

let js = {|
;; Shared definitions for all targets

(def false 0)
(def hash-map 0)
(def null 0)
(def true 0)
(def vector 0)

(defmacro not= [a b] (list 'not (list '= a b)))

;; Specific target prelude

;; JS prelude

(def __raw_template 0)
(def assoc 0)
(def new 0)
(def set! 0)
(def spread 0)

(defmacro FIXME [& args]
  (list 'throw
        (list 'Error.
              (concat
               (list
                'str
                (str "FIXME " __FILENAME__ ":" __LINE__ ":" (- __POSITION__ 1) " - "))
               args))))

(defmacro def- [k v] (list 'def ^:private k v))
(defmacro eprintln [& args] (concat (list '.error 'console) args))
(defmacro first [xs] (list 'get (list '.from 'Array xs) 0))
(defmacro number? [x] (list '= :number (list 'type x)))
(defmacro println [& args] (concat (list '.info 'console) args))
(defmacro rest [xs] (list '.toSpliced (list '.from 'Array xs) 0 1))
(defmacro seq? [x] (list 'Array.isArray x))
(defmacro some? [x] (list 'not (list 'nil? x)))
(defmacro str [& args] (concat (list '+ "") args))
(defmacro string? [x] (list '= :string (list 'type x)))

(defmacro reduce [f init xs]
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

(defmacro export-default [body] (list '__raw_template "export default " body))
(defmacro nil? [x] (list 'or (list '= 'null x) (list '= 'undefined x)))
(defmacro not [x] (list '__raw_template "!(" x ")"))
(defmacro throw [ex] (list '__raw_template "(function(){throw " ex "})()"))
(defmacro type [x] (list '__raw_template "typeof " x))

;; Collections
(defmacro assoc! [col key value] (list '__raw_template "" col "[" key "]=" value))
(defmacro concat [a b] (list '__raw_template "[..." a ", ..." b "]"))
(defmacro conj [a b] (list '__raw_template "[..." a ", " b "]"))
(defmacro cons [a b] (list '__raw_template "[" a ", ..." b "]"))
(defmacro get [target index] (list '__raw_template "" target "[" index "]"))
(defmacro merge [a b] (list '__raw_template "{ ..." a ", ..." b " }"))
(defmacro contains? [col key] (list '__raw_template "" key " in " col))

(defmacro quote_of_string [n]
  {:__y2k_type :quote
   :value n})

(defmacro quote? [x]
  (list '= :quote (list '.-__y2k_type x)))

(defmacro list [& args]
  (list 'let ['xs (concat (list 'vector) args)]
        (list 'set! (list '.-__y2k_type 'xs) :list)
        'xs))

(defmacro list? [x]
  (list '= :list (list '.-__y2k_type x)))

;; HTML

(def alert 0)
(def Array 0)
(def Buffer 0)
(def console 0)
(def createImageBitmap 0)
(def crypto 0)
(def Date 0)
(def document 0)
(def eval 0)
(def fetch 0)
(def globalThis 0)
(def JSON 0)
(def Math 0)
(def Object 0)
(def process 0)
(def Promise 0)
(def RegExp 0)
(def setTimeout 0)
(def undefined 0)
(def window 0)

;; Debug

(defmacro DEBUG [message value]
  (list 'do
        (list 'eprintln message value)
        value))

|}

let java = {|
;; Shared definitions for all targets

(def false 0)
(def hash-map 0)
(def null 0)
(def true 0)
(def vector 0)

(defmacro not= [a b] (list 'not (list '= a b)))

;; Specific target prelude

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
        (list 'if (list '= 'null 'result) default 'result)))

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
(defmacro nil? [x] (list '= 'null x))
(defmacro runnable [f] (list 'call-runtime ''runnable f))
(defmacro some? [x] (list 'not (list '= 'null x)))

|}

let bytecode = {|
;; Shared definitions for all targets

(def false 0)
(def hash-map 0)
(def null 0)
(def true 0)
(def vector 0)

(defmacro not= [a b] (list 'not (list '= a b)))

;; Specific target prelude

(def - 0)
(def * 0)
(def / 0)
(def + 0)
(def = 0)
(def atom 0)
(def concat 0)
(def deref 0)
(def get 0)
(def if 0)
(def list 0)
(def ns 0)
(def println 0)
(def reset! 0)
(def str 0)
(def vec 0)

(defmacro defonce [name value]
  (list 'if
        (list 'def name)
        'nil
        (list 'def name value)))

(defn not [x] (if x false true))

|}

let interpreter = {|
;; Shared definitions for all targets

(def false 0)
(def hash-map 0)
(def null 0)
(def true 0)
(def vector 0)

(defmacro not= [a b] (list 'not (list '= a b)))

;; Specific target prelude

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
(def FIXME 0)
(def get 0)
(def if 0)
(def list 0)
(def map 0)
(def map? 0)
(def println 0)
(def reduce 0)
(def some? 0)
(def str 0)
(def vec 0)
(def vector? 0)

(defn not [x] (if x false true))

|}
