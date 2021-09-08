(module
  ;; {}         :: dic
  ;; assoc      :: dic -> string -> _ -> dic
  ;; dic-get    :: dic -> string -> _
  ;; []         :: list
  ;; list-map   :: list -> (_ -> _) -> list
  ;; list-cons  :: _ -> list -> list
  ;; ref-create :: _ -> ref
  ;; ref-set    :: ref -> _ -> unit
  ;; ref-get    :: ref -> _
  ;; ui-text    :: dic -> widget
  ;; ui-edit    :: dic -> widget
  ;; ui-button  :: dic -> widget
  ;; ui-column  :: dic -> list -> widget
  ;; ui-render  :: widget -> unit

  (defn init []
    {:items [] :text ""})

  (defn update-text [new-text]
    (update-model (fn [model] (assoc model "text" new-text))))

  (defn add-item []
    (update-model
      (fn [model]
        (assoc
          (assoc model "text" "")
          "items"
          (list-cons
            (:text (ref-get model))
            (:items (ref-get model)))))))

  (defn render-item [text]
    (ui-text {:text text}))

  (defn view [model]
    (ui-column {}
      [ (ui-edit {:on-changed update-text :text (:text model)})
        (ui-button {:title "Add" :on-click add-item})
        (ui-column {} (list-map (:items model) render-item))])))
