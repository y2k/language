module TeaRunTests

let code =
    """
(module
  (def model (ref-create (dic-add (dic-add dic-nil "items" list-nil) "text" "")))

  (defn dispatch-render [] (ui-render (view (ref-get model))))

  (defn update-text [new-text]
    (ref-set model (dic-add (ref-get model) "text" new-text))
    (dispatch-render))

  (defn add-item []
    (ref-set
      model
      (dic-add
        (ref-get model)
        "items"
        (list-cons
          (dic-get (ref-get model) "text")
          (dic-get (ref-get model) "items"))))
    (ref-set model (dic-add (ref-get model) "text" ""))
    (dispatch-render))

  (defn render-item [text]
    (ui-text (dic-add dic-nil "text" text)))

  (defn view [model]
    (ui-column dic-nil
      (list-cons
        (ui-edit (dic-add (dic-add dic-nil "on-changed" update-text) "text" (dic-get model "text")))
        (list-cons
          (ui-button (dic-add (dic-add dic-nil "title" "Add") "on-click" add-item)))
          (list-cons
            (ui-column dic-nil (list-map (dic-get model "items") render-item))
            list-nil))))

  (defn main []
    (dispatch-render)))
"""

open Xunit
open Swensen.Unquote
open MetaLang

module P = LanguageParser

[<Fact>]
let test () =
    let prog =
        P.compile code
        |> mapToCoreLang
        |> TestUtils.resolveTypes

    ()
