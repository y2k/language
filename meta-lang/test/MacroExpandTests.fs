module MacroExpandTests

open Xunit
open Swensen.Unquote
open MetaLang

[<Fact>]
let test () =
    let actual =
        Call(
            "case",
            [ Symbol "x"
              Symbol "a"
              Symbol "r_a"
              Symbol "b"
              Symbol "r_b"
              Symbol "r_def" ]
        )
        |> MacroExpand.run

    let expected =
        Let(
            [ ("__var__", Symbol "x") ],
            [ Call(
                  "if",
                  [ Call("=", [ Symbol "__var__"; Symbol "a" ])
                    Symbol "r_a"
                    Call("if", [ Call("=", [ Symbol "__var__"; Symbol "b" ]); Symbol "r_b"; Symbol "r_def" ]) ]
              ) ]
        )

    test <@ expected = actual @>
