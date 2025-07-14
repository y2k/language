module A = Alcotest
module Js = Core__.Backend_js

let compile code = Js.compile ~log:true ~filename:"main.clj" code |> Printf.sprintf "%s;\n\nprocess.exit(main());"

let run_code code =
  let path = Filename.temp_file "test_" ".js" in
  prerr_endline @@ "===| CODE |===\n" ^ code ^ "\n==============\n";
  Out_channel.(with_open_bin path (fun f -> output_string f code));
  Sys.command (Printf.sprintf "node %s" path) |> string_of_int

let create_test =
  List.map (fun (loc, (input : string), expected) ->
      A.test_case loc `Slow (fun () ->
          let compiled = compile input in
          let actual = run_code compiled in
          A.check A.string "" expected actual))

let tests =
  ( "JS",
    [
      (__LOC__, {|(defn main [] (+ (first (rest [1 40 3])) (count (rest [1 40 3]))))|}, "42");
      (__LOC__, {|(defn main [] (last [1 2 42]))|}, "42");
      (__LOC__, {|(defn main [] (+ (get (conj [1 2] 40) 2) (count (conj [1 2] 4))))|}, "43");
      (__LOC__, {|(defn main [] (if (fn? (fn [] 0)) 42 0))|}, "42");
      (__LOC__, {|(defn main [] (if (= __LOC__ "main.clj:1:22") 42 0))|}, "42");
      (__LOC__, {|(defn f [a] (fn [] (if (= "/cat" (:b a)) 42 3))) (defn main [] ((f {:b "/cat"})))|}, "42");
      (* *)
      (__LOC__, {|(defn main [] (first [42 1 2]))|}, "42");
      (__LOC__, {|(def a (atom 1)) (defn main [] (reset! a 2) (swap! a (fn [x] (+ x 1))) (deref a))|}, "3");
      (__LOC__, {|(defn main [] (+ (case 9 1 11 2 22 100) (case 2 1 11 2 22 3 33 44)) )|}, "122");
      (__LOC__, {|(defn main [] (reduce (fn [a [_ b]] (+ a b)) 0 {:a 40 :b 2}))|}, "42");
      (__LOC__, {|(defn main [] (+ (cond false 1 :else 40) (cond false 1 true 2 :else 3)))|}, "42");
      (__LOC__, {|(defn main [] (reduce (fn [a b] (+ a b)) 0 [10 20 12]))|}, "42");
      ( __LOC__,
        {|(defn main [] (if (and (nil? nil) (number? 3) (boolean? false) (string? "") (vector? [1 2 3])) 42 0))|},
        "42" );
      (__LOC__, {|(defn main [] (:b (assoc {:a 1 :b 2 :c 3} :b 42)) )|}, "42");
      (__LOC__, {|(defn main [] (count (concat [1 2] [1] [1 2 3])) )|}, "6");
      (__LOC__, {|(defn main [] (+ ( * 2 100) (- 50 10) (+ 1 2 2)))|}, "245");
      (__LOC__, {|(defn f [] (FIXME "A" 2 "C")) (defn main [] 42)|}, "42");
      (__LOC__, {|(defn main [] (+ (if (string? "x") 40 0) (if (string? 1) 0 2)))|}, "42");
      (__LOC__, {|(defn main [] (+ (if (some? "x") 40 0) (if (nil? "x") 0 2)))|}, "42");
      (__LOC__, {|(defn- f [x] x) (defn main [] (f 42))|}, "42");
      (__LOC__, {|(defn main [] (if (not false) 42 0))|}, "42");
      (__LOC__, {|(defn main [] (if (and (< 1 2) (<= 3 4) (> 6 5) (>= 8 7)) 42 0))|}, "42");
      (__LOC__, {|(defn f [[{a :b c :d} e]] (+ a c e)) (defn main [] (f [{:b 10 :f 30 :d 20} 12]))|}, "42");
      (__LOC__, {|(defn f [_ {a :b c :d} _] (+ a c)) (defn main [] (f 1 {:b 40 :d 2} 3) )|}, "42");
      (__LOC__, {|(defn f [_ [a b] _] (+ a b)) (defn main [] (f 1 [40 2] 3) )|}, "42");
      (__LOC__, {|(defn f [a b & xs] (+ a b (count xs))) (defn main [] (f 30 10 7 8) )|}, "42");
      (__LOC__, {|(defn main [] (parseInt (.toString (Number. 42))))|}, "42");
      (__LOC__, {|(defn main [] (:b {:a 1 :b 42 :c 3}))|}, "42");
      (__LOC__, {|(defn main [] (+ (if (empty? []) 40 0) (if (empty? [1]) 0 2)))|}, "42");
      (__LOC__, {|(defn main [] (parseInt (.toString "42")))|}, "42");
      (__LOC__, {|(defn main [] (let [a (atom 2)] (reset! a 42) (deref a)))|}, "42");
      (__LOC__, {|(defn main [] (parseInt (str "4" :2)))|}, "42");
      (__LOC__, {|(defn main [] (+ (if true 21 1) (if false 1 21)))|}, "42");
      (__LOC__, {|(defn main [] (let [x 42] x))|}, "42");
      (__LOC__, {|(defn f [a b] (eprintln a b) (println a b) (+ a b)) (defn main [] (f 40 2))|}, "42");
      (__LOC__, {|(defn main [] 42)|}, "42");
    ]
    |> create_test )
