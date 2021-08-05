module Tests

open Xunit
open Swensen.Unquote
open MetaLang

let assetCode exptected actualNode =
    let extTypes = ExternalTypeResolver.loadDefault ()

    let ctx =
        TypeResolver.defaultContext
        |> TypeResolver.registerFunc "dic/get" ([ Specific "dic/t"; Specific "string" ], Unknown)
        |> TypeResolver.registerFunc "int/add" ([ Specific "int/t"; Specific "int/t" ], Unknown)

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
let ``function add int`` () =
    modules [] [
        defn "foo" [ "a"; "b" ] [ Call("int/add", [ Symbol "a"; Symbol "b" ]) ]
    ]
    |> assetCode
        """(module
;; int/t -> int/t -> ???
(defn foo [a b] (int/add a b)))"""

[<Fact>]
let ``call function`` () =
    modules [] [
        defn "foo" [ "a" ] [ Call("int/add", [ Symbol "a"; Symbol "b" ]) ]
        defn "bar" [ "a" ] [ Call("foo", [ Call("foo", [ Symbol "a" ]) ]) ]
    ]
    |> assetCode
        """(module
;; int/t -> ???
(defn foo [a] (int/add a b))
;; int/t -> ???
(defn bar [a] (foo (foo a))))"""
