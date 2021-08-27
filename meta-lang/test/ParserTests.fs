module ParserTests

open Xunit
open Swensen.Unquote
open MetaLang

module P = LanguageParser

[<Fact>]
let test3 () =
    let node =
        P.compile
            """
    (module
      (defn dispatch-render [] (ui-render (view (ref-get model)))))"""

    test
        <@ ExtModule [ ExtDefn(
                           "dispatch-render",
                           [],
                           Unknown,
                           [ ExtCall("ui-render", [ ExtCall("view", [ ExtCall("ref-get", [ ExtSymbol "model" ]) ]) ]) ]
                       ) ] = node @>

[<Fact>]
let test2 () =
    let node =
        P.compile "(module (defn main [a b] (foo (baz a b) b) (bar a b)))"

    test
        <@ ExtModule [ ExtDefn(
                           "main",
                           [ "a", Unknown; "b", Unknown ],
                           Unknown,
                           [ ExtCall(
                               "foo",
                               [ ExtCall("baz", [ ExtSymbol "a"; ExtSymbol "b" ])
                                 ExtSymbol "b" ]
                             )
                             ExtCall("bar", [ ExtSymbol "a"; ExtSymbol "b" ]) ]
                       ) ] = node @>

[<Fact>]
let test () =
    let node = P.compile "(module)"
    test <@ ExtModule [] = node @>
