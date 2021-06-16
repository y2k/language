module Tests

open Xunit
open Swensen.Unquote
open MetaLang

let assetCode exptected actualNode =
    let extTypes = ExternalTypeResolver.loadDefault ()

    let ctx =
        TypeResolver.defaultContext
        |> TypeResolver.registerFunc "dic/get" [ Specific "dic/t"; Specific "string" ]

    let actual =
        actualNode
        |> TypeResolver.resolve' extTypes ctx
        |> LispRenderer.render

    test <@ exptected = actual @>

[<Fact>]
let ``empty module`` () = assetCode "(module)" (modules [] [])

[<Fact>]
let ``function`` () =
    assetCode
        """(module
;; ??? -> ??? -> ???
(defn add [a b] a))"""
        (modules [] [
            defn "add" [ "a"; "b" ] [ Symbol "a" ]
         ])

[<Fact>]
let ``read dictionary`` () =
    modules [] [
        defn "foo" [ "a"; "b" ] [ rdic "{:f b}" ]
    ]
    |> assetCode
        """(module
;; ??? -> dic/t -> ???
(defn foo [a b] (dic/get b "f")))"""
