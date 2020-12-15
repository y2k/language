module TypeResolver

open MetaLang
module E = ExternalTypeResolver

type ResolvedInfo = Map<string, Type>

let rec private resolve' (ext: E.t) program: Node * ResolvedInfo =
    let resolve = resolve' ext

    match program with
    | Defn (name, ps, body) ->
        let ri =
            body
            |> List.fold
                (fun a node ->
                    let (_, ri) = resolve node
                    ri |> Map.fold (fun a1 k v -> Map.add k v a1) a)
                Map.empty

        let nps =
            ps
            |> List.map
                (fun (x, t) ->
                    match t with
                    | Unknown -> x, Map.tryFind x ri |> Option.defaultValue Unknown
                    | Specific _ -> x, t)

        Defn(name, nps, body), Map.empty
    | Call ("intrinsic_invoke_static", (String path) :: args) ->
        let ri =
            args
            |> List.mapi (fun i n -> n, E.resolveStatic' ext path (List.length args) i)
            |> List.fold
                (fun a (x, type') ->
                    match x with
                    | Symbol name -> Map.add name (Specific type') a
                    | _ -> a)
                Map.empty

        program, ri
    | Module (_, nodes) -> nodes |> List.map resolve |> List.last
    | other -> other, Map.empty

let resolve ext program = resolve' ext program |> fst
