module MacroExpandTests

open Xunit
open Swensen.Unquote
open MetaLang

let private validate actual expected =
    let actual =
        actual
        |> LanguageParser.parse
        |> MacroExpand.expandSexp
        |> LanguageParser.compileToExtNode
        |> mapToCoreLang
        |> MacroExpand.run

    let expected = expected |> LanguageParser.compile |> mapToCoreLang
    test <@ expected = actual @>

[<Fact>]
let test2 () =
    validate "(module (defn foo [] (->> a (b) (c d))))" "(module (defn foo [] (c d (b a))))"

[<Fact>]
let test () =
    validate
        "(module (defn foo [] (case x a r_a b r_b r_def)))"
        "(module (defn foo [] (let [__var__ x] (if (= __var__ a) r_a (if (= __var__ b) r_b r_def)) ) ))"
