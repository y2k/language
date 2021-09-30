(module

 (defn main [env]
   (->
    (str "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=" (get-in env [:env :token]) "&tag=cat")
    (download-json)
    (get-in [:data :image_mp4_url])))

 (defn on-loaded [json]
   (get-in [:data :image_mp4_url]))

 (defn main [env]
   (let [token (get-in env [:env :token])
         url (str "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=" token "&tag=cat")]
     [:download-json {:url url :on-complete on-loaded}])))
