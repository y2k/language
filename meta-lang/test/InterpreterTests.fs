module InterpreterTests

open Swensen.Unquote

[<Xunit.Fact>]
let test4 () =
    """
    (module (defn main [a b] (+ a b)))
    """
    |> LanguageParser.compile
    |> MetaLang.mapToCoreLang
    |> Interpreter.run "main" [ box 1; box 2 ]
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
    |> Interpreter.run "main" [ box 1; box 2 ]
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
    |> Interpreter.run "main" [ box 1; box 2 ]
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
    |> Interpreter.run "main" [ box 1; box 2 ]
    |> fun actual ->
        let actual = unbox actual
        test <@ 1 = actual @>
