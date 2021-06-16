module Tests

open Xunit
open Swensen.Unquote

[<Fact>]
let ``lisp compiler test`` () =
    test <@ 2 = 3 @>
