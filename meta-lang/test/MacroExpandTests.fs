module MacroExpandTests

open Xunit
open Swensen.Unquote
open MetaLang

let private validate actual expected =
    let actual =
        actual
        |> sprintf "(module (defn foo [] %s))"
        |> LanguageParser.parse
        |> MacroExpand.expandSexp
        |> LanguageParser.compileToExtNode
        |> mapToCoreLang
        |> MacroExpand.run

    let expected =
        expected
        |> sprintf "(module (defn foo [] %s))"
        |> LanguageParser.compile
        |> mapToCoreLang

    test <@ expected = actual @>

[<Fact>]
let test3 () =
    validate "(:foo bar)" " (get bar :foo)"
    validate "(:foo (let [x {}] x))" " (get (let [x {}] x) :foo)"

[<Fact>]
let test2 () =
    validate "(->> a (b) (c d))" " (c d (b a))"

[<Fact>]
let test () =
    validate "(case x a r_a b r_b r_def)" "(let [__var__ x] (if (= __var__ a) r_a (if (= __var__ b) r_b r_def)))"
