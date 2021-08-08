module ConditionTests

open Xunit
open Swensen.Unquote
open MetaLang

[<Fact>]
let test () =
    let actual =
        modules [] [
            defn "main" [ "c"; "a"; "b" ] [ call "if" [ Symbol "c"; Symbol "a"; Symbol "b" ] ]
        ]
        |> TestUtils.resolveTypes

    let expected =
        modules [] [
            Defn(
                "main",
                [ "c", Specific "bool"
                  "a", Unknown
                  "b", Unknown ],
                Unknown,
                [ call "if" [ Symbol "c"; Symbol "a"; Symbol "b" ] ]
            )
        ]
        |> TestUtils.resolveTypes

    test <@ expected = actual @>
