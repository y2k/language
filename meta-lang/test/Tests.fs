module Tests

open Xunit
open Swensen.Unquote
open MetaLang

[<Fact>]
let ``empty module`` () =
    let actual = modules [] [] |> LispRenderer.render
    test <@ "(module)" = actual @>

[<Fact>]
let ``function`` () =
    let actual =
        modules [] [
            defn "add" [ "a"; "b" ] [ Symbol "a" ]
        ]
        |> (TypeResolver.resolve (ExternalTypeResolver.loadDefault ()))
        |> LispRenderer.render

    let exptected = """(module
;; ??? -> ??? -> ???
(defn add [a b] a))"""

    test <@ exptected = actual @>

[<Fact>]
let ``read dictionary`` () =
    let actual =
        modules [] [
            defn "add" [ "a"; "b" ] [ ReadDic("f", Symbol "b") ]
        ]
        |> (TypeResolver.resolve (ExternalTypeResolver.loadDefault ()))
        |> LispRenderer.render

    let exptected = """(module
;; ??? -> Dictionary -> ???
(defn add [a b] {:f b}))"""

    test <@ exptected = actual @>
