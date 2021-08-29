module InterpreterTests

[<Xunit.Fact>]
let test () =
    """
    (module (defn main [a b] a))
    """
    |> LanguageParser.compile
    |> MetaLang.mapToCoreLang
    |> Interpreter.run "main" [ box 1; box 2 ]
    |> ignore

()
