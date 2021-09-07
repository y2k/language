module InterpreterTests

open Xunit
open Swensen.Unquote
open MetaLang

let asset code args expected =
    let foregnFunctions =
        Map.ofList [ "+",
                     (fun (args: obj list) ->
                         let toInt (arg: obj) =
                             match arg with
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
        let actual = unbox actual
        test <@ expected = actual @>

[<Fact>]
let test13 () =
    let asset code args expected =
        let foregnFunctions =
            Map.ofList [ "+",
                         (fun (args: obj list) ->
                             let toInt (arg: obj) =
                                 match arg with
                                 | :? int as x -> x
                                 | :? RSexp as x -> let (RSexp x) = x in int x
                                 | x -> failwithf "Can't parse '%O' to int" x

                             let a = toInt args.[0]
                             let b = toInt args.[1]
                             a + b |> box) ]

        code
        |> LanguageParser.compile
        |> mapToCoreLang
        |> failwithf "%O"
        |> ignore

    asset "(module (defn foo [f] (f 42)) (defn main [] (foo (fn [a] a))))" [] (RSexp "42")

// [<Fact>]
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
