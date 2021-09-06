module InterpreterTests

module TestInterpreter =
    let run =
        Map.ofList [ "+",
                     (fun (args: obj list) ->
                         (unbox<int> args.[0]) + (unbox<int> args.[1])
                         |> box) ]
        |> Interpreter.run

open Swensen.Unquote

[<Xunit.Fact>]
let test7 () =
    """
    (module (def foo 42) (defn main [] foo))
    """
    |> LanguageParser.compile
    |> MetaLang.mapToCoreLang
    |> TestInterpreter.run "main" []
    |> fun actual ->
        let actual = unbox actual
        test <@ "42" = actual @>

[<Xunit.Fact>]
let test6 () =
    """
    (module (defn main [] 42))
    """
    |> LanguageParser.compile
    |> MetaLang.mapToCoreLang
    |> TestInterpreter.run "main" []
    |> fun actual ->
        let actual = unbox actual
        test <@ "42" = actual @>

[<Xunit.Fact>]
let test5 () =
    """
    (module (defn main [] "hello_world"))
    """
    |> LanguageParser.compile
    |> MetaLang.mapToCoreLang
    |> TestInterpreter.run "main" []
    |> fun actual ->
        let actual = unbox actual
        test <@ "\"hello_world\"" = actual @>

[<Xunit.Fact>]
let test4 () =
    """
    (module (defn main [a b] (+ a b)))
    """
    |> LanguageParser.compile
    |> MetaLang.mapToCoreLang
    |> TestInterpreter.run "main" [ box 1; box 2 ]
    |> fun actual ->
        let actual = unbox actual
        test <@ 3 = actual @>

[<Xunit.Fact>]
let test3 () =
    """
    (module (defn foo [c d] c) (defn main [a b] (foo a b)))
    """
    |> LanguageParser.compile
    |> MetaLang.mapToCoreLang
    |> TestInterpreter.run "main" [ box 1; box 2 ]
    |> fun actual ->
        let actual = unbox actual
        test <@ 1 = actual @>

[<Xunit.Fact>]
let test2 () =
    """
    (module (defn main [a b] b))
    """
    |> LanguageParser.compile
    |> MetaLang.mapToCoreLang
    |> TestInterpreter.run "main" [ box 1; box 2 ]
    |> fun actual ->
        let actual = unbox actual
        test <@ 2 = actual @>

[<Xunit.Fact>]
let test () =
    """
    (module (defn main [a b] a))
    """
    |> LanguageParser.compile
    |> MetaLang.mapToCoreLang
    |> TestInterpreter.run "main" [ box 1; box 2 ]
    |> fun actual ->
        let actual = unbox actual
        test <@ 1 = actual @>
