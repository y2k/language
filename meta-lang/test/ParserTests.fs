module ParserTests

open Xunit
open Swensen.Unquote
open MetaLang

module P = LanguageParser

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
