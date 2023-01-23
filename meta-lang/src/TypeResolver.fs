module TypeResolver

open MetaLang
module E = ExternalTypeResolver

type ResolvedInfo = Map<string, Type>

type private ArgsDef =
    | GeneralArgs of Type list
    | MultiArgs of Type

type Context =
    private
        { functions: Map<string, ArgsDef * Type>
          funParams: (string * Type) list }

    static member empty =
        { functions = Map.empty
          funParams = [] }

module Map =
    let addAll newXs xs =
        Map.fold (fun xs k v -> Map.add k v xs) xs newXs

let fundFunctionByArgs (ctx: Context) args retType =
    ctx.functions
    |> Map.tryPick (fun name (args', rt) ->
        match args' with
        | GeneralArgs xs when xs = args && rt = retType -> Some(name, retType)
        | MultiArgs x when (List.replicate (Seq.length args) x) = args && rt = retType -> Some(name, retType)
        | _ -> None)

let findReturnType (ctx: Context) (inputType: Type) =
    ctx.functions
    |> Map.pick (fun _ (args, retType) ->
        match args with
        | GeneralArgs [ x ] when x = inputType -> Some retType
        | MultiArgs x when x = inputType -> Some retType
        | _ -> None)

let findFuncArgType (ctx: Context) name argIndex =
    Map.tryFind name ctx.functions
    |> Option.map fst
    |> Option.map (function
        | GeneralArgs x -> List.item argIndex x
        | MultiArgs x -> x)
    |> Option.defaultWith (fun _ -> failwithf "Cant find func: %O[%O]" name argIndex)

let rec private resolve' (ext: E.t) (ctx: Context) program : Node * ResolvedInfo =
    let resolve = resolve' ext

    match program with
    | Module (imports, nodes) ->
        let (_, nodes) =
            nodes
            |> List.fold
                (fun (ctx, nodes) node ->
                    let ctx =
                        { ctx with
                            functions =
                                match node with
                                | Defn (name, ps, _, _) ->
                                    let types = ps |> List.map snd
                                    Map.add name (GeneralArgs types, Unknown) ctx.functions
                                | _ -> ctx.functions }

                    let (node', _) = resolve ctx node

                    let ctx' =
                        { ctx with
                            functions =
                                match node' with
                                | Defn (name, ps, _, _) ->
                                    let types = ps |> List.map snd
                                    Map.add name (GeneralArgs types, Unknown) ctx.functions
                                | _ -> ctx.functions }

                    ctx', nodes @ [ node' ])
                ({ Context.empty with functions = ctx.functions }, [])

        Module(imports, nodes), Map.empty
    | Defn (name, ps, _, body) ->
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
            |> List.map (fun (x, t) ->
                match t with
                | Unknown -> x, Map.tryFind x ri |> Option.defaultValue Unknown
                | Dictionary _
                | Specific _ -> x, t
                | Function _ -> failwith "TODO")

        Defn(name, nps, Unknown, body), Map.empty
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
                |> fst

            match funcSign with
            | GeneralArgs x when Seq.length args <> Seq.length x ->
                failwithf
                    "Used args count %i when call fun '%s' not equals that required %i"
                    (Seq.length args)
                    callName
                    (Seq.length x)
            | _ -> ()

            let ri =
                args
                |> List.mapi (fun i n ->
                    n,
                    match funcSign with
                    | GeneralArgs funcSign -> funcSign.[i]
                    | MultiArgs x -> x)
                |> List.fold
                    (fun a (x, type') ->
                        match x with
                        | Symbol name -> Map.add name type' a
                        | node ->
                            let (_, r) = resolve ctx node
                            Map.addAll r a)
                    Map.empty

            program, ri
    | Let (bindings, body) ->
        let body = body |> List.map (fun x -> resolve ctx x |> fst)
        let bindings = List.map (fun (k, v) -> k, resolve ctx v |> fst) bindings
        Let(bindings, body), Map.empty
    | NMap map -> NMap(Map.map (fun _ x -> resolve ctx x |> fst) map), Map.empty
    | NVector xs -> NVector(List.map (fun x -> resolve ctx x |> fst) xs), Map.empty
    | Symbol _ as other -> other, Map.empty
    | Fn (args, resType, body) ->
        let body = body |> List.map (fun x -> resolve ctx x |> fst)
        Fn(args, resType, body), Map.empty
    | Def _ as other -> other, Map.empty
    | Const _ as other -> other, Map.empty

let registerFunc name (args, retType) (ctx: Context) =
    { ctx with functions = ctx.functions |> Map.add name (GeneralArgs args, retType) }

let registerVarArgsFunc name arg retType (ctx: Context) =
    { ctx with functions = ctx.functions |> Map.add name (MultiArgs arg, retType) }

let defaultContext =
    { Context.empty with
        functions =
            Map.ofArray
                [| "intrinsic_set", (GeneralArgs [ (Dictionary Map.empty); Specific "String"; Unknown ], Unknown)
                   "add",
                   (GeneralArgs[Specific "Int"
                                Specific "Int"],
                    Unknown) |] }
    |> registerFunc "if" ([ Specific "bool"; Unknown; Unknown ], Unknown)
    |> registerFunc "map" ([ Function([ Unknown ], Unknown); Specific "list" ], Specific "list")
    |> registerFunc "FIXME" ([], Unknown)
    |> registerFunc "=" ([ Unknown; Unknown ], Specific "bool")
    |> registerFunc "unknown_of_sexp" ([ RawSexp ], Unknown)

let resolve ext ctx program = resolve' ext ctx program |> fst
