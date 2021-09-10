module InterpreterTests

open Xunit
open Swensen.Unquote
open MetaLang

let asset code args expected =
    let foregnFunctions =
        Map.ofList [ "if",
                     (fun (args: (unit -> obj) list) ->
                         let condition =
                             match args.[0] () with
                             | :? bool as b -> b
                             | :? RSexp as x -> let (RSexp x) = x in System.Boolean.Parse(x)
                             | x -> failwithf "Can't parse '%O' to bool" x

                         if condition then
                             args.[1] ()
                         else
                             args.[2] ())
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

    code
    |> LanguageParser.compile
    |> mapToCoreLang
    |> Interpreter.run foregnFunctions "main" args
    |> fun actual ->
        let expected = box expected
        test <@ expected = actual @>

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
    asset
        "(module (defn main [] [1 2 3]))"
        []
        [ RSexp "1" |> box
          RSexp "2" |> box
          RSexp "3" |> box ]

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
