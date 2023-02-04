module MacroExpand

open MetaLang
open LanguageParser

let private expandSingleSexp (name: string) (body: sexp list) : sexp =
    match name with
    | "->>" ->
        body
        |> List.reduce (fun a x ->
            match x with
            | List xs -> xs @ [ a ] |> List
            | _ -> failwithf "invalid macro body: %O" body)
    | _ -> Atom name :: body |> List

let rec expandSexp (node: sexp) : sexp =
    match node with
    | Atom _ -> node
    | List ((Atom head) :: atoms) -> expandSingleSexp head (List.map expandSexp atoms)
    | List atoms -> List.map expandSexp atoms |> List
    | Vector atoms -> List.map expandSexp atoms |> Vector
    | SMap atoms -> List.map expandSexp atoms |> SMap

let private expand name (args: Node list) : Node =
    match name with
    | "case" ->
        let xs =
            args
            |> Seq.skip 1
            |> Seq.chunkBySize 2
            |> fun xs ->
                Seq.foldBack
                    (fun (xs: Node array) (ac: Node) ->
                        match xs with
                        | [| a; b |] -> Call("if", [ Call("=", [ Symbol "__var__"; a ]); b; ac ])
                        | _ -> xs.[0])
                    xs
                    (Call("unreached", []))

        Let([ "__var__", args[0] ], [ xs ])
    | _ -> Call(name, args)

let rec run (node: Node) : Node =
    match node with
    | Const x -> Const x
    | Symbol x -> Symbol x
    | Call (name, args) -> expand name (args |> List.map run)
    | Def (a, b) -> Def(a, run b)
    | Defn (a, b, c, d) -> Defn(a, b, c, d |> List.map run)
    | Module (_, a) -> Module([], a |> List.map run)
    | Fn (b, c, d) -> Fn(b, c, d |> List.map run)
    | NVector xs -> NVector(xs |> List.map run)
    | NMap xs -> NMap(xs |> Map.map (fun _ x -> run x))
    | Let (xs, body) -> Let(xs |> List.map (fun (k, v) -> k, run v), body |> List.map run)
