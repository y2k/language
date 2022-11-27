module ParserTests

open Xunit
open Swensen.Unquote
open MetaLang

module P = LanguageParser

[<Fact>]
let test9 () =
    let node = P.compile "(module (defn foo [x] (= 2 2)))"

    test
        <@
            ExtModule [ ExtDefn("foo", [ ("x", Unknown) ], Unknown, [ ExtCall("=", [ ExtConst "2"; ExtConst "2" ]) ]) ] = node
        @>

[<Fact>]
let test8_2 () =
    let node =
        P.compile """(module (defn a [x] x) #_(comment (println "hello") (println "world")) (defn b [x] x))"""

    test
        <@
            ExtModule
                [ ExtDefn("a", [ ("x", Unknown) ], Unknown, [ ExtSymbol "x" ])
                  ExtDefn("b", [ ("x", Unknown) ], Unknown, [ ExtSymbol "x" ]) ] = node
        @>

[<Fact>]
let test8_1 () =
    let node = P.compile "(module #_ (defn main [a]))"
    test <@ ExtModule [] = node @>

[<Fact>]
let test8 () =
    let node = P.compile "(module #_(defn main [a]))"
    test <@ ExtModule [] = node @>

[<Fact>]
let test7 () =
    let node = P.compile "(module (defn main [a] (. a substring 1 10)))"

    test
        <@
            ExtModule
                [ ExtDefn(
                      "main",
                      [ ("a", Unknown) ],
                      Unknown,
                      [ ExtCall(".", [ ExtSymbol "a"; ExtSymbol "substring"; ExtConst "1"; ExtConst "10" ]) ]
                  ) ] = node
        @>

[<Fact>]
let test6 () =
    let node = P.compile "(module (defn main [a] (. a toUpperCase)))"

    test
        <@
            ExtModule
                [ ExtDefn(
                      "main",
                      [ ("a", Unknown) ],
                      Unknown,
                      [ ExtCall(".", [ ExtSymbol "a"; ExtSymbol "toUpperCase" ]) ]
                  ) ] = node
        @>

[<Fact>]
let test5 () =
    let node =
        P.compile
            """
    (module
      (defn update-text [new-text]
        (ref-set model (dic-add (ref-get model) "text" new-text))
        (dispatch-render)))"""

    test
        <@
            ExtModule
                [ ExtDefn(
                      "update-text",
                      [ ("new-text", Unknown) ],
                      Unknown,
                      [ ExtCall(
                            "ref-set",
                            [ ExtSymbol "model"
                              ExtCall(
                                  "dic-add",
                                  [ ExtCall("ref-get", [ ExtSymbol "model" ])
                                    ExtConst "\"text\""
                                    ExtSymbol "new-text" ]
                              ) ]
                        )
                        ExtCall("dispatch-render", []) ]
                  ) ] = node
        @>

[<Fact>]
let test4 () =
    let node =
        P.compile
            """
    (module
      (def model (ref-create (dic-add (dic-add dic-nil "items" list-nil) "text" ""))))"""

    test
        <@
            ExtModule
                [ ExtDef(
                      "model",
                      ExtCall(
                          "ref-create",
                          [ ExtCall(
                                "dic-add",
                                [ ExtCall(
                                      "dic-add",
                                      [ ExtSymbol "dic-nil"; ExtConst "\"items\""; ExtSymbol "list-nil" ]
                                  )
                                  ExtConst "\"text\""
                                  ExtConst "\"\"" ]
                            ) ]
                      )
                  ) ] = node
        @>

[<Fact>]
let test3 () =
    let node =
        P.compile
            """
    (module
      (defn dispatch-render [] (ui-render (view (ref-get model)))))"""

    test
        <@
            ExtModule
                [ ExtDefn(
                      "dispatch-render",
                      [],
                      Unknown,
                      [ ExtCall("ui-render", [ ExtCall("view", [ ExtCall("ref-get", [ ExtSymbol "model" ]) ]) ]) ]
                  ) ] = node
        @>

[<Fact>]
let test2 () =
    let node = P.compile "(module (defn main [a b] (foo (baz a b) b) (bar a b)))"

    test
        <@
            ExtModule
                [ ExtDefn(
                      "main",
                      [ "a", Unknown; "b", Unknown ],
                      Unknown,
                      [ ExtCall("foo", [ ExtCall("baz", [ ExtSymbol "a"; ExtSymbol "b" ]); ExtSymbol "b" ])
                        ExtCall("bar", [ ExtSymbol "a"; ExtSymbol "b" ]) ]
                  ) ] = node
        @>

[<Fact>]
let test () =
    let node = P.compile "(module)"
    test <@ ExtModule [] = node @>
