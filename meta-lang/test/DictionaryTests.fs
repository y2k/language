module DictionaryTests

open Xunit
open Swensen.Unquote
open MetaLang

[<Fact>]
let test () =
    let actual =
        modules [] [
            defn "main" [ "a" ] [ call "dic-get" [ Symbol "a"; Const "b" ] ]
        ]
        |> TestUtils.resolveTypes

    let expected =
        modules [] [
            Defn("main", [ "a", Specific "dic" ], Unknown, [ call "dic-get" [ Symbol "a"; Const "b" ] ])
        ]

    test <@ expected = actual @>
