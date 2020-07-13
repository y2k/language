(ns extractor.core (:gen-class)
    (:require [clojure.data.json :as json]))

(comment
  (reset! global {})
  (let [home (java.lang.System/getenv "HOME")]
    (loadAndScanJar
     (java.io.File. (str home "/Projects/language/extractor/__data/android.jar"))
     (str home "/out.json")
     10))
;
  )

(defn loadAndScanJar [jarFile outFile limit]
  (let [jar (java.util.jar.JarFile. jarFile)]
    (->>
     (.entries jar)
     (enumeration-seq)
     (filter #(and (.endsWith (.getName %1) ".class") (not= (.getName %1) "java/lang/Object.class")))
     (map #(extractClassFromStream (.getInputStream jar %1)))
     (take limit)
     (pprint)
     (spit outFile))))

(def global (atom {}))

(defn extractClassFromStream [stream]
  (let [cr (org.objectweb.asm.ClassReader. stream)
        className (atom "")
        methods (atom [])]
    (.accept
     cr
     (proxy [org.objectweb.asm.ClassVisitor]
            [org.objectweb.asm.Opcodes/ASM6]
       (visit [version access name signature superName interfaces]
         (reset! className (toClsName name)))
       (visitMethod [access name descriptor signature exceptions]
        ;  (reset! global {:access access :name name :descriptor descriptor :signature signature})
         (if (not= "<clinit>" name)
           (swap! methods #(conj %1 {:name name :static (isStatic access)})))
         nil))
     org.objectweb.asm.ClassReader/SKIP_DEBUG)
    {:name @className :methods @methods}))

(defn isStatic [x] (not= 0 (bit-and org.objectweb.asm.Opcodes/ACC_STATIC x)))
(defn toClsName [x] = (.replace x \/ \.))
(defn pprint [x] (with-out-str (json/pprint x)))

(defn -main [& args] (println "Hello, World!"))
