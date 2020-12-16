module TypeResolver

open MetaLang
module E = ExternalTypeResolver

type ResolvedInfo = Map<string, Type>
type Context = { functions: Map<string, Type list> }

let rec private resolve' (ext: E.t) (ctx: Context) program: Node * ResolvedInfo =
    let resolve = resolve' ext

    match program with
    | Module (imports, nodes) ->
        let (_, nodes) =
            nodes
            |> List.fold
                (fun (ctx, nodes) node ->
                    let (node', _) = resolve ctx node

                    let ctx' =
                        { ctx with
                              functions =
                                  match node' with
                                  | Defn (name, ps, _) ->
                                      let types = ps |> List.map snd
                                      Map.add name types ctx.functions
                                  | _ -> ctx.functions }

                    ctx', nodes @ [ node' ])
                ({ functions = Map.empty }, [])

        Module(imports, nodes), Map.empty
    | Defn (name, ps, body) ->
        let ri =
            body
            |> List.fold
                (fun a node ->
                    let (_, ri) = resolve ctx node
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
    | Call (callName, args) ->
        let funcSign = Map.find callName ctx.functions

        let ri =
            args
            |> List.mapi (fun i n -> n, funcSign.[i])
            |> List.fold
                (fun a (x, type') ->
                    match x with
                    | Symbol name -> Map.add name type' a
                    | _ -> a)
                Map.empty

        program, ri
    | other -> other, Map.empty

let resolve ext program =
    resolve' ext { functions = Map.empty } program
    |> fst
