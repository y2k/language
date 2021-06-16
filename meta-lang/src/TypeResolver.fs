module TypeResolver

open MetaLang
module E = ExternalTypeResolver

type ResolvedInfo = Map<string, Type>

type Context =
    private
        { functions: Map<string, Type list>
          funParams: (string * Type) list }
    static member empty =
        { functions = Map.empty
          funParams = [] }

module Map =
    let addAll newXs xs =
        Map.fold (fun xs k v -> Map.add k v xs) xs newXs

let rec private resolve'' (ext: E.t) (ctx: Context) program : Node * ResolvedInfo =
    let resolve = resolve'' ext

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
                ({ Context.empty with
                       functions = ctx.functions },
                 [])

        Module(imports, nodes), Map.empty
    | Defn (name, ps, body) ->
        let funcCtx = { ctx with funParams = ps }

        let ri =
            body
            |> List.fold
                (fun a node ->
                    let (_, ri) = resolve funcCtx node
                    ri |> Map.fold (fun a1 k v -> Map.add k v a1) a)
                Map.empty

        let nps =
            ps
            |> List.map
                (fun (x, t) ->
                    match t with
                    | Unknown -> x, Map.tryFind x ri |> Option.defaultValue Unknown
                    | Dictionary _
                    | Specific _ -> x, t
                    | Function _ -> failwith "TODO")

        Defn(name, nps, body), Map.empty
    | Dic items ->
        let ri =
            items
            |> List.map snd
            |> List.fold
                (fun a n ->
                    let (_, r) = resolve ctx n
                    Map.addAll r a)
                Map.empty

        program, ri
    | ReadDic (_, Symbol dicName) ->
        let ri =
            Map.ofList [ dicName, Dictionary Map.empty ]

        program, ri
    | Bind (_, nodes) ->
        let ri =
            nodes
            |> List.fold
                (fun a n ->
                    let (_, r) = resolve ctx n
                    Map.addAll r a)
                Map.empty

        program, ri
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
    | Call ("intrinsic_invoke", (String path) :: _ :: args) ->
        let ri =
            args
            |> List.mapi (fun i n -> n, E.resolve' ext path (List.length args) i)
            |> List.fold
                (fun a (x, type') ->
                    match x with
                    | Symbol name -> Map.add name (Specific type') a
                    | node ->
                        let (_, r) = resolve ctx node
                        Map.addAll r a)
                Map.empty

        program, ri
    | Call (callName, args) ->
        match List.tryFind (fun (n, _) -> n = callName) ctx.funParams with
        | Some (_, ftype) ->
            match ftype with
            | Unknown ->
                let ri =
                    Map.ofList [ callName, Function(args |> List.map (fun _ -> Unknown), Unknown) ]

                program, ri
            | Specific _
            | Dictionary _ -> failwithf "Call unsupported type (%O)" ftype
            | Function _ -> failwith "???"
        | None ->
            let funcSign =
                Map.tryFind callName ctx.functions
                |> Option.defaultWith (fun _ -> failwithf "Can't find function '%s' (%A)" callName ctx)

            let ri =
                args
                |> List.mapi (fun i n -> n, funcSign.[i])
                |> List.fold
                    (fun a (x, type') ->
                        match x with
                        | Symbol name -> Map.add name type' a
                        | node ->
                            let (_, r) = resolve ctx node
                            Map.addAll r a)
                    Map.empty

            program, ri
    | other -> other, Map.empty

let defaultContext =
    { Context.empty with
          functions =
              Map.ofArray [| "intrinsic_set",
                             [ (Dictionary Map.empty)
                               Specific "String"
                               Unknown ]
                             "add", [ Specific "Int"; Specific "Int" ] |] }

let resolve ext program =
    resolve'' ext defaultContext program |> fst

let resolve' ext ctx program = resolve'' ext ctx program |> fst

let registerFunc name args (ctx: Context) =
    { ctx with
          functions = ctx.functions |> Map.add name args }
