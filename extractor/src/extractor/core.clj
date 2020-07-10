(ns extractor.core (:gen-class)
    (:require [clojure.data.json :as json]))

(comment
  (let [home (java.lang.System/getenv "HOME")]
       (loadAndScanJar
        (java.io.File. (str home "/Projects/language/extractor/__data/android.jar"))
        (str home "/out.json")))
;
  )

(defn loadAndScanJar [jarFile outFile]
  (let [jar (java.util.jar.JarFile. jarFile)]
    (->>
     (.entries jar)
     (enumeration-seq)
     (filter (fn [x] (and (.endsWith (.getName x) ".class") (not= (.getName x) "java/lang/Object.class"))))
     (map #(visitClassFile (.getInputStream jar %1)))
     (filter #(not= {} %1))
     (json/write-str)
     (spit outFile))))

(defn visitClassFile [stream]
  (let [cr (org.objectweb.asm.ClassReader. stream)
        skip (atom false)
        lastClassName (atom "")
        classes (atom {})]
    (.accept
     cr
     (proxy [org.objectweb.asm.ClassVisitor]
            [org.objectweb.asm.Opcodes/ASM6]
       (visit [version access name signature superName interfaces]
         (if (or (.contains name "$")
                 (= (and access org.objectweb.asm.Opcodes/ACC_PUBLIC) 0)
                 (= (toClsName superName) "java.lang.Object"))
           (reset! skip true)
           (do
             (reset! skip false)
             (reset! lastClassName (toClsName name)))))
       (visitMethod [access name descriptor signature exceptions]
         (if (not @skip)
           (swap! classes #(let [methods (conj (or (:methods %1) []) name)
                                 class (or (:class %1) @lastClassName)]
                             (assoc %1 :class class :methods methods))))
         nil))
     org.objectweb.asm.ClassReader/SKIP_DEBUG)
    @classes))

(defn toClsName [x] = (.replace x \/ \.))

(defn -main [& args] (println "Hello, World!"))
