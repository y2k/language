module MacroExpand

open LanguageParser

let private expandSingleSexp (name: string) (body: sexp list) : sexp =
    match name, body with
    | "case", _ ->
        let xs =
            body
            |> Seq.skip 1
            |> Seq.chunkBySize 2
            |> fun xs ->
                Seq.foldBack
                    (fun x a ->
                        match x with
                        | [| x; y |] ->
                            List
                                [ Atom "if"
                                  List[Atom "="
                                       Atom "__var__"
                                       x]
                                  y
                                  a ]
                        | x -> x[0])
                    xs
                    (Atom "")

        List [ Atom "let"; Vector [ Atom "__var__"; body[0] ]; xs ]
    | "comment", _ -> Atom "nil"
    | "if-some", _ ->
        match body with
        | [ Vector [ Atom _ as var; src ]; then_; else_ ] ->
            List[Atom "let"

                 Vector[var
                        src]

                 List [ Atom "if"; List [ Atom "some?"; var ]; then_; else_ ]]
        | _ -> failwithf "invalid macro body: %O" body
    | "->>", _ ->
        body
        |> List.reduce (fun a x ->
            match x with
            | List xs -> xs @ [ a ] |> List
            | _ -> failwithf "invalid macro body: %O" body)
    | x, [ dict ] when x.StartsWith ':' -> List [ Atom "get"; dict; Atom name ]
    | _ -> Atom name :: body |> List

let rec expandSexp (node: sexp) : sexp =
    match node with
    | Atom _ -> node
    | List ((Atom head) :: atoms) -> expandSingleSexp head (List.map expandSexp atoms)
    | List atoms -> List.map expandSexp atoms |> List
    | Vector atoms -> List.map expandSexp atoms |> Vector
    | SMap atoms -> List.map expandSexp atoms |> SMap
