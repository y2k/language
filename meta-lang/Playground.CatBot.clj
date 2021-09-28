(module

 (defn on-loaded [json]
   (let [url (:image_mp4_url (:data json))]
     [:send-messge {:text url}]))

 (defn main [env]
   (let [token (:token (:env env))
         url (str "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=" token "&tag=cat")]
     [:download-json {:url url :on-complete on-loaded}])))
