module rec MacroExpandTests

open Xunit
open Swensen.Unquote
open MetaLang

[<Fact>]
let test5 () =
    validate "(if-some [x (foo)] 1 2)" "(let [x (foo)] (if (some? x) 1 2))"
    validate "(if-some [x (foo)] x false)" "(let [x (foo)] (if (some? x) x false))"

[<Fact>]
let test4 () =
    validate "(comment (let [x 1] x))" "nil"

[<Fact>]
let test3 () =
    validate "(:foo bar)" "(get bar :foo)"
    validate "(:foo (let [x {}] x))" "(get (let [x {}] x) :foo)"

[<Fact>]
let test2 () =
    validate "(->> a (b) (c d))" "(c d (b a))"

[<Fact>]
let test1 () =
    validate "(case x a r_a b r_b r_def)" "(let [__var__ x] (if (= __var__ a) r_a (if (= __var__ b) r_b r_def)))"

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
