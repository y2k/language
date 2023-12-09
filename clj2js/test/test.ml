let log code =
  print_endline "";
  code |> Clj2js.main |> print_endline;
  print_endline "\n============"

let () =
  {|(defn fetch [request env context]
   (.log console request)
   (.log console request))|}
  |> log

let () = {|(defn fetch [request env context]
     (->
       a b))|} |> log

let () =
  {|(defn fetch [request env context]
     (->
      (.json request null)
      (.next (fn [text] (failwith "???")))))|}
  |> log

let () = {|(defn fetch [request env context]
     request)|} |> log

let () =
  {|(let [pTags (-> (.querySelectorAll document "p") Array/from (.filter (fn [x] (= x.innerText "Tags"))) first)
   parent (-> pTags .parentElement .nextElementSibling .firstElementChild)
   children (Array/from (.children parent))]
   (.sort children
        (fn [n1 n2]
          (defn tag_count [n]
            (-> n.children second .innerText (.replaceAll "," "") parseInt))
          (- (tag_count n1) (tag_count n2))))
   (while parent.firstChild
   (.removeChild parent (.firstChild parent)))
   (.forEach
   children
   (fn [child] (.appendChild parent child))))|}
  |> log

let () =
  {|(defn fetch-handler [request env context] request)

  (export-default {:fetch fetch-handler})|}
  |> log

let () = {|(Response. "hello_world" 1 false)|} |> log

let () = {|(comment 1 2 3)
  (println 1 2 3)
  (comment 1 2 3)|} |> log

let () =
  {|(export-default {:foo 1 :foo2 {:foo 1 :bar "2" :baz false} :bar "2" :baz false})|}
  |> log
