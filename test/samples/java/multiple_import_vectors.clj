;; 2024-01-02:00000000-0000-0000-0000-000000000000
(ns app.java_import
  (:import [java.time LocalDate] [java.util UUID]))

(defn test []
  (str (LocalDate/of 2024 1 2)
       ":"
       (UUID/fromString "00000000-0000-0000-0000-000000000000")))
