(defproject app "0.1.0"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.jsoup/jsoup "1.15.3"]
                 [http-kit/http-kit "2.6.0"]
                 [garden/garden "1.3.10"]]
  :repl-options {:init-ns app.research-framework})