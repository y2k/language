(ns extractor.core (:gen-class)
    (:require [clojure.data.json :as json]))

(defn pprint [x] (with-out-str (json/pprint x)))
(defn saveObjectToFile [outFile x] (spit outFile (pprint x)))
(defn isStatic [x] (not= 0 (bit-and org.objectweb.asm.Opcodes/ACC_STATIC x)))
(defn toClsName [x] (.replace x \/ \.))
(defn filenameToClsName [x] (toClsName (.replace x ".class" "")))

(defn getParams [method]
  (->>
   (.getParameterTypes method)
   (map #(do {:type (.getCanonicalName %1)}))))

(defn getMethods [class]
  (->>
   (.getMethods class)
   (map #(do {:name (.getName %1)
              :is-static (java.lang.reflect.Modifier/isStatic (.getModifiers %1))
              :return (.getCanonicalName (.getReturnType %1)) :params (getParams %1)}))))

(defn handleEntity [classLoader entry]
  (let [name (filenameToClsName (.getName entry))
        class (.loadClass classLoader name)]
    {:name name :methods (getMethods class)}))

(defn extractAllClasses [jarFile outFile]
  (let [jar (java.util.jar.JarFile. jarFile)
        classLoader (java.net.URLClassLoader. (into-array [(.toURL (.toURI jarFile))]))]
    (->>
     (.entries jar)
     (enumeration-seq)
     (filter #(and (.endsWith (.getName %1) ".class") (not= (.getName %1) "java/lang/Object.class")))
     (filter #(= "android.widget.Toast" (filenameToClsName (.getName %1))))
     (map #(handleEntity classLoader %1))
     (saveObjectToFile outFile))))

(comment
  (let [home (java.lang.System/getenv "HOME")
        jarFile (java.io.File. (str home "/Projects/language/extractor/__data/android-4.1.1.4.jar"))
        outFile (str home "/out.json")]
    (extractAllClasses jarFile outFile))
;
  )

(defn -main [& args] (println "Hello, World!"))
