module NumberTests

open Xunit
open Swensen.Unquote
open MetaLang

[<Fact>]
let test5 () =
    let actual =
        modules [] [
            defn "foo" [ "a"; "b" ] [ call "+" [ Symbol "a"; Symbol "b" ] ]
            defn "main" [ "a"; "b" ] [ call "foo" [ Symbol "a"; Symbol "b" ] ]
        ]
        |> TestUtils.resolveTypes

    let expected =
        modules [] [
            Defn(
                "foo",
                [ "a", Specific "int"
                  "b", Specific "int" ],
                Unknown,
                [ call "+" [ Symbol "a"; Symbol "b" ] ]
            )
            Defn(
                "main",
                [ "a", Specific "int"
                  "b", Specific "int" ],
                Unknown,
                [ call "foo" [ Symbol "a"; Symbol "b" ] ]
            )
        ]

    test <@ actual = expected @>

[<Fact>]
let test4 () =
    let actual =
        try
            modules [] [
                defn "foo" [ "a" ] [ call "+" [ Const "2.0"; Symbol "a" ] ]
            ]
            |> TestUtils.resolveTypes
            |> Some
        with
        | :? System.FormatException -> None

    let expected: Node option = None

    test <@ expected = actual @>

[<Fact>]
let test3 () =
    let actual =
        modules [] [
            defn "foo" [ "a" ] [ call "+" [ Const "2"; Symbol "a" ] ]
        ]
        |> TestUtils.resolveTypes

    let expected =
        modules [] [
            Defn("foo", [ "a", Specific "int" ], Unknown, [ call "+" [ Const "2"; Symbol "a" ] ])
        ]

    test <@ actual = expected @>

[<Fact>]
let test2 () =
    let actual =
        modules [] [
            defn "foo" [ "a"; "b" ] [ call "+." [ Symbol "a"; Symbol "b" ] ]
        ]
        |> TestUtils.resolveTypes

    let expected =
        modules [] [
            Defn(
                "foo",
                [ "a", Specific "float"
                  "b", Specific "float" ],
                Unknown,
                [ call "+." [ Symbol "a"; Symbol "b" ] ]
            )
        ]

    test <@ actual = expected @>

[<Fact>]
let test1 () =
    let actual =
        modules [] [
            defn "foo" [ "a"; "b" ] [ call "+" [ Symbol "a"; Symbol "b" ] ]
        ]
        |> TestUtils.resolveTypes

    let expected =
        modules [] [
            Defn(
                "foo",
                [ "a", Specific "int"
                  "b", Specific "int" ],
                Unknown,
                [ call "+" [ Symbol "a"; Symbol "b" ] ]
            )
        ]

    test <@ actual = expected @>
