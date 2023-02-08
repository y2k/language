module rec InterpreterTests

open Xunit
open Swensen.Unquote
open MetaLang

[<Fact>]
let test37 () =
    asset
        "(module (def count (make-count)) (defn foo [] (inc-and-ret count)) (defn main [] (if-some [x 1] (do (foo) (foo) count) (do (foo) (foo) count))))"
        []
        (ref 2)

[<Fact>]
let test36 () =
    asset "(module (defn main [] (do 1 2 3)))" [] (RSexp "3")
    asset "(module (defn main [x] (do 1 2 x)))" [ 3 ] 3

[<Fact>]
let test35 () =
    asset "(module (defn main [] (vector? [])))" [] true
    asset "(module (defn main [] (vector? [1 2 3])))" [] true
    asset "(module (defn main [] (vector? [\"1\" \"2\" \"3\"])))" [] true
    asset "(module (defn main [x] (vector? x)))" [ [ box 0; box 1; box 2 ] ] true
    asset "(module (defn main [] (vector? \"hello\")))" [] false
    asset "(module (defn main [x] (vector? x)))" [ "test" ] false

[<Fact>]
let test34 () =
    asset "(module (defn foo [f] (f)) (defn main [] (foo (fn [] 3))))" [] (RSexp "3")
    asset "(module (defn foo [f] (f)) (defn main [x] (foo (fn [] x))))" [ 3 ] 3
    asset "(module (defn foo [f] (f)) (defn main [] (let [x 3] (foo (fn [] x)))))" [] (RSexp "3")
    asset "(module (defn foo [f] (f)) (defn main [x] (let [x 3] (foo (fn [] x)))))" [ 2 ] (RSexp "3")
    asset "(module (defn foo [f] (f)) (defn main [x] (let [x 3] (let [x 4] (foo (fn [] x))))))" [ 2 ] (RSexp "4")

[<Fact>]
let test33 () =
    asset "(module (defn main [] (or nil false 3)))" [] (RSexp "3")
    asset "(module (defn main [x] (or nil false x)))" [ 3 ] 3
    asset "(module (defn main [] (or true 3)))" [] (RSexp "true")
    asset "(module (defn main [x] (or true x)))" [ 3 ] (RSexp "true")
    asset "(module (defn main [x] (or nil false x 3)))" [ true ] true
    asset "(module (defn main [x] (or nil false x 3)))" [ false ] (RSexp "3")

[<Fact>]
let test32 () =
    asset "(module (comment foo bar) (defn foo [f x] (f x)) (defn main [] (foo (fn [x] x) 3)))" [] (RSexp "3")
    asset "(module (defn foo [f x] (f x)) (defn main [] (foo (fn [x] x) 3)))" [] (RSexp "3")
    asset "(module (defn foo [f x] (f x)) (defn main [] (comment foo bar) (foo (fn [x] x) 3)))" [] (RSexp "3")

[<Fact>]
let test31 () =
    asset "(module (defn foo [f x] (f x)) (defn main [] (foo (fn [x] x) 3)))" [] (RSexp "3")
    asset "(module (defn foo [f x] (f x)) (defn main [] (let [x 2] (foo (fn [a] a) 3))))" [] (RSexp "3")

[<Fact>]
let test30 () =
    asset "(module (defn main [] (let [x 1] (let [x 2] x))))" [] (RSexp "2")
    asset "(module (defn main [] (let [x 1] (let [x 2 x 3] x))))" [] (RSexp "3")
    asset "(module (defn main [x] (let [x 2] (let [x 3] x))))" [ 1 ] (RSexp "3")
    asset "(module (defn main [x] (let [x 2] (let [x 3 x 4] x))))" [ 1 ] (RSexp "4")

[<Fact>]
let test29 () =
    asset "(module (defn main [x] (get-in {:a {:b x}} [:a :b])))" [ 42 ] 42

[<Fact>]
let test28 () =
    asset "(module (defn main [] (str \"foo\" 42 \"bar\")))" [] "foo42bar"

[<Fact>]
let test27 () =
    asset
        "(module (defn main [] [:download {:to_url \"g.com\"}]))"
        []
        [ box <| RSexp "download"
          box <| Map.ofList [ "to_url", box <| RSexp "\"g.com\"" ] ]

[<Fact>]
let test26 () =
    asset "(module (defn main [] {:a 1 :b \"2\"}))" [] (Map.ofList [ "a", box <| RSexp "1"; "b", box <| RSexp "\"2\"" ])

[<Fact>]
let test25 () =
    asset "(module (defn main [] (let [a 42] a)))" [] (RSexp "42")
    asset "(module (defn main [] (let [a 42 b 3] b)))" [] (RSexp "3")
    asset "(module (defn foo [a] a) (defn main [x] (let [b (foo x)] b)))" [ 42 ] 42

[<Fact>]
let test24 () =
    asset "(module (defn main [a b] (if true a b)))" [ 3; 5 ] 3
    asset "(module (defn main [a b] (if false a b)))" [ 3; 5 ] 5
    asset "(module (defn main [c a b] (if c a b)))" [ true; 3; 5 ] 3
    asset "(module (defn main [c a b] (if c a b)))" [ false; 3; 5 ] 5

[<Fact>]
let test23 () =
    asset "(module (def foo :bar) (defn main [] foo))" [] (RSexp "bar")

[<Fact>]
let test22 () =
    asset "(module (def foo :bar) (defn main [] foo 0))" [] (RSexp "0")

[<Fact>]
let test21 () =
    asset "(module (defn main [b] (:a {:a b})))" [ 42 ] 42

[<Fact>]
let test20 () =
    asset "(module (defn main [] (:a {:a 42})))" [] (RSexp "42")

[<Fact>]
let test19 () =
    asset "(module (def foo (:a {:a 1 :b \"2\"})) (defn main [] foo 0))" [] (RSexp "0")

[<Fact>]
let test18 () =
    asset "(module (def foo {:a 1 :b \"2\"}) (defn main [] foo 0))" [] (RSexp "0")

[<Fact>]
let test17 () =
    asset "(module (defn main [a b c] [a b c]))" [ 1; 2; 3 ] [ box 1; box 2; box 3 ]

[<Fact>]
let test16 () =
    asset "(module (defn main [] [1 2 3]))" [] [ RSexp "1" |> box; RSexp "2" |> box; RSexp "3" |> box ]

[<Fact>]
let test15 () =
    asset "(module (def c 11) (defn foo [f] (f 5)) (defn main [b] (foo (fn [a] (+ a (+ b c))))))" [ 3 ] 19

[<Fact>]
let test14 () =
    asset "(module (defn foo [f] (f 5)) (defn main [b] (foo (fn [a] (+ a b)))))" [ 3 ] 8

[<Fact>]
let test13 () =
    asset "(module (defn foo [f] (f 0)) (defn main [b] (foo (fn [a] b))))" [ 3 ] 3

[<Fact>]
let test12 () =
    asset "(module (defn foo [f] (f 42)) (defn main [] (foo (fn [a] a))))" [] (RSexp "42")

[<Fact>]
let test11 () =
    asset "(module (defn foo [f] (f 42)) (defn bar [a] a) (defn main [] (foo bar)))" [] (RSexp "42")

[<Fact>]
let test10 () =
    asset "(module (def foo 2) (defn main [a] (+ foo a)))" [ box 2 ] 4

[<Fact>]
let test9 () =
    asset "(module (defn main [a] (+ 2 a)))" [ box 2 ] 4

[<Fact>]
let test8 () =
    asset "(module (defn main [] (+ 2 2)))" [] 4

[<Fact>]
let test7 () =
    asset "(module (def foo 42) (defn main [] foo))" [] (RSexp "42")

[<Fact>]
let test6 () =
    asset "(module (defn main [] 42))" [] (RSexp "42")

[<Fact>]
let test5 () =
    asset "(module (defn main [] \"hello_world\"))" [] (RSexp "\"hello_world\"")

[<Fact>]
let test4 () =
    asset "(module (defn main [a b] (+ a b)))" [ box 1; box 2 ] 3

[<Fact>]
let test3 () =
    asset "(module (defn foo [c d] c) (defn main [a b] (foo a b)))" [ box 1; box 2 ] 1

[<Fact>]
let test2 () =
    asset "(module (defn main [a b] b))" [ box 1; box 2 ] 2

[<Fact>]
let test1 () =
    asset "(module (defn main [a b] a))" [ box 1; box 2 ] 1

let asset code (args: obj list) (expected: obj) =
    let foregnFunctions =
        Map.ofList
            [ "get-in",
              (fun (args: (unit -> obj) list) ->
                  let m: Map<string, obj> = args.[0] () |> unbox
                  let path: obj list = args.[1] () |> unbox

                  path
                  |> List.fold
                      (fun ma k' ->
                          let m: Map<string, obj> = unbox ma

                          let k =
                              match k' with
                              | :? RSexp as x -> let (RSexp x) = x in x
                              | x -> failwithf "Can't parse '%O' (%O) to string" x (x.GetType())

                          m.[unbox k])
                      (box m)
                  |> box)
              "make-count", (fun (args: (unit -> obj) list) -> ref 0 |> box)
              "inc-and-ret",
              (fun (args: (unit -> obj) list) ->
                  let r: int ref = args[0]() |> unbox
                  r.Value <- r.Value + 1
                  r.Value |> box)
              "str",
              (fun (args: (unit -> obj) list) ->
                  args
                  |> List.map (fun f ->
                      match f () with
                      | :? string as s -> s
                      | :? bool as b -> b.ToString()
                      | :? RSexp as x -> let (RSexp x) = x in x.Trim('"').ToString()
                      | x -> failwithf "Can't parse '%O' (%O) to string" x (x.GetType()))
                  |> List.fold (sprintf "%O%O") ""
                  |> box)
              "if",
              (fun (args: (unit -> obj) list) ->
                  let condition =
                      match args.[0] () with
                      | :? bool as b -> b
                      | :? RSexp as x -> let (RSexp x) = x in System.Boolean.Parse(x)
                      | x -> failwithf "Can't parse '%O' to bool" x

                  if condition then args.[1] () else args.[2] ())
              "+",
              (fun (args: (unit -> obj) list) ->
                  let toInt (arg: (unit -> obj)) =
                      match arg () with
                      | :? int as x -> x
                      | :? RSexp as x -> let (RSexp x) = x in int x
                      | x -> failwithf "Can't parse '%O' to int" x

                  let a = toInt args.[0]
                  let b = toInt args.[1]
                  a + b |> box) ]

    let ctx =
        DefaultFunctions.defaultContext
        |> TypeResolver.registerFunc "make-count" ([], Unknown)
        |> TypeResolver.registerFunc "inc-and-ret" ([ Unknown ], Unknown)

    code
    |> LanguageParser.parse
    |> MacroExpand.expandSexp
    |> LanguageParser.compileToExtNode
    |> mapToCoreLang
    |> TypeResolver.resolve (ExternalTypeResolver.loadDefault ()) ctx
    |> Interpreter.run
        (fun x _ ->
            Map.tryFind x foregnFunctions
            |> Option.orElseWith (fun _ -> DefaultFunctions.findNativeFunction x ()))
        "main"
        args
    |> fun actual ->
        let expected = box expected
        test <@ expected = actual @>
