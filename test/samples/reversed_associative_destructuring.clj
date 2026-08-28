;; https://example.test-GET-first-token-options-2-ready

(defn describe [{url :url
                 [tag] :tags
                 {{auth :auth} :headers method :method} :request}]
  (let [{props :props {retry :retry} :options}
        {:props "options" :options {:retry "2"}}
        {:status status}
        {:status "ready"}]
    (str url "-" method "-" tag "-" auth "-" props "-" retry "-" status)))

(defn test []
  (describe {:url "https://example.test"
             :tags ["first"]
             :request {:method "GET" :headers {:auth "token"}}}))
