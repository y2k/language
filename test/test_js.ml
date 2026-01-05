module Js = Backend__.Backend_js

let compile ~filename code =
  let prelude_path = Unix.realpath "../../../prelude/data/y2k/rt.js" in
  Js.compile ~builtin_macro:Macro.invoke ~log:true ~filename ~prelude_path code
  |> Printf.sprintf "%s;\n\nprocess.exit(test());"

let run_code code =
  let path = Filename.temp_file "test_" ".js" in
  prerr_endline @@ "===| CODE |===\n" ^ code ^ "\n==============\n";
  Out_channel.(with_open_bin path (fun f -> output_string f code));
  Sys.command (Printf.sprintf "node %s" path) |> string_of_int

let create_test ~filename speed tests =
  tests
  |> List.map (fun (loc, input, expected) ->
      Alcotest.test_case loc speed (fun () ->
          let compiled = compile ~filename input in
          let actual = run_code compiled in
          Alcotest.(check string) "" expected actual))

let tests =
  [
    ( __LOC__,
      {|(defn test [] (if (and (> (unixtime) 1700000000) (< (unixtime) 2700000000)) 42 2))|},
      "42" );
    ( __LOC__,
      {|(defn test [] (+ (first (rest [1 40 3])) (count (rest [1 40 3]))))|},
      "42" );
    (__LOC__, {|(defn test [] (last [1 2 42]))|}, "42");
    ( __LOC__,
      {|(defn test [] (+ (get (conj [1 2] 40) 2) (count (conj [1 2] 4))))|},
      "43" );
    (__LOC__, {|(defn test [] (if (fn? (fn [] 0)) 42 0))|}, "42");
    (__LOC__, {|(defn test [] (if (= __LOC__ "main.clj:1:22") 42 0))|}, "42");
    ( __LOC__,
      {|(defn f [a] (fn [] (if (= "/cat" (:b a)) 42 3))) (defn test [] ((f {:b "/cat"})))|},
      "42" );
    (__LOC__, {|(defn test [] (first [42 1 2]))|}, "42");
    ( __LOC__,
      {|(def a (atom 1)) (defn test [] (reset! a 2) (swap! a (fn [x] (+ x 1))) (deref a))|},
      "3" );
    ( __LOC__,
      {|(defn test [] (+ (case 9 1 11 2 22 100) (case 2 1 11 2 22 3 33 44)) )|},
      "122" );
    ( __LOC__,
      {|(defn test [] (reduce (fn [a [_ b]] (+ a b)) 0 {:a 40 :b 2}))|},
      "42" );
    ( __LOC__,
      {|(defn test [] (+ (cond false 1 :else 40) (cond false 1 true 2 :else 3)))|},
      "42" );
    (__LOC__, {|(defn test [] (reduce (fn [a b] (+ a b)) 0 [10 20 12]))|}, "42");
    ( __LOC__,
      {|(defn test [] (if (and (nil? nil) (number? 3) (boolean? false) (string? "") (vector? [1 2 3])) 42 0))|},
      "42" );
    (__LOC__, {|(defn test [] (:b (assoc {:a 1 :b 2 :c 3} :b 42)) )|}, "42");
    (__LOC__, {|(defn test [] (count (concat [1 2] [1] [1 2 3])) )|}, "6");
    (__LOC__, {|(defn test [] (+ ( * 2 100) (- 50 10) (+ 1 2 2)))|}, "245");
    (__LOC__, {|(defn f [] (FIXME "A" 2 "C")) (defn test [] 42)|}, "42");
    ( __LOC__,
      {|(defn test [] (+ (if (string? "x") 40 0) (if (string? 1) 0 2)))|},
      "42" );
    ( __LOC__,
      {|(defn test [] (+ (if (some? "x") 40 0) (if (nil? "x") 0 2)))|},
      "42" );
    (__LOC__, {|(defn- f [x] x) (defn test [] (f 42))|}, "42");
    (__LOC__, {|(defn test [] (if (not false) 42 0))|}, "42");
    ( __LOC__,
      {|(defn test [] (if (and (< 1 2) (<= 3 4) (> 6 5) (>= 8 7)) 42 0))|},
      "42" );
    ( __LOC__,
      {|(defn f [[{a :b c :d} e]] (+ a c e)) (defn test [] (f [{:b 10 :f 30 :d 20} 12]))|},
      "42" );
    ( __LOC__,
      {|(defn f [_ {a :b c :d} _] (+ a c)) (defn test [] (f 1 {:b 40 :d 2} 3) )|},
      "42" );
    ( __LOC__,
      {|(defn f [_ [a b] _] (+ a b)) (defn test [] (f 1 [40 2] 3) )|},
      "42" );
    ( __LOC__,
      {|(defn f [a b & xs] (+ a b (count xs))) (defn test [] (f 30 10 7 8) )|},
      "42" );
    (__LOC__, {|(defn test [] (parseInt (.toString (Number. 42))))|}, "42");
    (__LOC__, {|(defn test [] (:b {:a 1 :b 42 :c 3}))|}, "42");
    ( __LOC__,
      {|(defn test [] (+ (if (empty? []) 40 0) (if (empty? [1]) 0 2)))|},
      "42" );
    (__LOC__, {|(defn test [] (parseInt (.toString "42")))|}, "42");
    ( __LOC__,
      {|(defn test [] (let [a (atom 2)] (reset! a 42) (deref a)))|},
      "42" );
    (__LOC__, {|(defn test [] (parseInt (str "4" :2)))|}, "42");
    (__LOC__, {|(defn test [] (+ (if true 21 1) (if false 1 21)))|}, "42");
    (__LOC__, {|(defn test [] (let [x 42] x))|}, "42");
    ( __LOC__,
      {|(defn f [a b] (eprintln a b) (println a b) (+ a b)) (defn test [] (f 40 2))|},
      "42" );
    (__LOC__, {|(defn test [] 42)|}, "42");
  ]
  |> create_test ~filename:"main.clj" `Slow
