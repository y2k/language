module LambdaTests

(*
  (def init
    (fn [] (dic-add (dic-add dic-nil "items" list-nil) "text" "")))
*)

open MetaLang

[<Xunit.Fact>]
let test () =
    let code =
        """
(module
  (def add
    (fn [a b] (+ a b))))"""

    ()

    LanguageParser.compile code
    |> mapToCoreLang
    |> TestUtils.resolveTypes
