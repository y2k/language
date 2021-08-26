module LispRenderer

open MetaLang

let private reduceSafe' emptyValue f =
    function
    | [] -> emptyValue
    | xs -> List.reduce f xs

let private reduceSafe f = reduceSafe' "" f

let rec private renderType =
    function
    | Unknown -> "???"
    | Specific name -> name
    | Dictionary _ -> "Dictionary"
    | Function (args, retType) ->
        sprintf
            "(%s -> %s)"
            (args
             |> List.map renderType
             |> reduceSafe (sprintf "%s -> %s"))
            (renderType retType)

let rec render =
    function
    | Symbol sym -> sym
    // | Bind (args, body) ->
    //     sprintf
    //         "(let [%s] %s)"
    //         (args
    //          |> List.map (fun (k, v) -> sprintf "%s %s" k (render v))
    //          |> reduceSafe (sprintf "%s %s"))
    //         (body
    //          |> List.map render
    //          |> reduceSafe (sprintf "%s %s"))
    | Call (name, args) ->
        sprintf
            "(%s %s)"
            name
            (args
             |> List.map render
             |> reduceSafe (sprintf "%s %s"))
    | Def (name, body) -> sprintf "(def %s %s)" name (render body)
    | Defn (name, ps, _, body) ->
        sprintf
            "\n;; %s -> ???\n(defn %s [%s] %s)"
            (ps
             |> List.map snd
             |> List.map renderType
             |> reduceSafe' "()" (sprintf "%s -> %s"))
            name
            (ps |> List.map fst |> reduceSafe (sprintf "%s %s"))
            (body
             |> List.map render
             |> reduceSafe (sprintf "%s %s"))
    | Module (_, body) ->
        body
        |> List.map render
        |> reduceSafe (sprintf "%s%s")
        |> sprintf "(module%s)"
