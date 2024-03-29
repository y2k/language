module TeaRunTests

let code =
    """
(module
  (defn init []
    (dic-add (dic-add dic-nil "items" list-nil) "text" ""))

  (defn update-text [new-text]
    (update-model (fn [model] (dic-add model "text" new-text))))

  (defn add-item []
    (update-model
      (fn [model]
        (dic-add
            (dic-add model "text" "")
            "items"
            (list-cons
              (dic-get (ref-get model) "text")
              (dic-get (ref-get model) "items"))))))

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
            list-nil)))))
"""

open MetaLang

module P = LanguageParser

// [<Xunit.Fact>]
let test () =
    let prog =
        P.compile code
        |> mapToCoreLang
        |> TestUtils.resolveTypes

    ()
