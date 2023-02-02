module Interpreter

open MetaLang

let private tryFindFunction funcs funcName funArgs =
    funcs
    |> List.tryPick (function
        | Defn (n, argTypes, retType, body) when n = funcName && (List.length argTypes = List.length funArgs) ->
            Some(argTypes, retType, body)
        | _ -> None)

let private findFunction funcs funcName funArgs =
    tryFindFunction funcs funcName funArgs
    |> Option.defaultWith (fun _ -> failwithf "Can't find function '%s' with %i args" funcName (Seq.length funArgs))

type Context =
    private
        { funcs: Node list
          extFuncResolve: string -> int -> ((unit -> obj) list -> obj) option
          funArgs: obj list
          localVariables: Map<string, obj>
          argTypes: (string * Type) list
          globalValues: Map<string, obj> }

let findValueInContext (ctx: Context) symName : obj =
    let findInArgs () =
        ctx.argTypes
        |> List.tryFindIndex (fun (n, _) -> n = symName)
        |> Option.map (fun argIndex -> ctx.funArgs.[argIndex])

    Map.tryFind symName ctx.localVariables
    |> Option.orElseWith (fun _ -> findInArgs ())
    |> Option.orElseWith (fun _ -> Map.tryFind symName ctx.globalValues)
    |> Option.defaultWith (fun _ -> failwithf "Can't find value '%s'" symName)

let rec private invokeNode (ctx: Context) (body: Node) : obj =
    match body with
    | Let (letArgs, body) ->
        let ctx =
            letArgs
            |> List.fold
                (fun ctx (name, letArgBody) ->
                    { ctx with localVariables = Map.add name (invokeNode ctx letArgBody) ctx.localVariables })
                ctx

        body |> List.map (invokeNode ctx) |> List.last
    | Symbol symName -> findValueInContext ctx symName
    | Call (callFunName, callArgs) ->
        let funInArgs =
            ctx.argTypes
            |> List.tryFindIndex (fun (n, _) -> n = callFunName)
            |> Option.bind (fun i ->
                match ctx.funArgs.[i] with
                | :? Node as n -> Some n
                | _ -> None)
            |> Option.map (function
                | Defn (_, argTypes, retType, body) -> argTypes, retType, body
                | x -> failwithf "Argument %s must be functions, but it %O" callFunName x)

        match funInArgs with
        | Some (fargTypes, _, fbody) ->
            let ctx2 =
                { ctx with
                    argTypes = fargTypes
                    funArgs = callArgs |> List.map (fun argBody -> invokeNode ctx argBody) }

            fbody |> List.map (fun b -> invokeNode ctx2 b) |> List.last
        | None ->
            let lambda =
                ctx.argTypes
                |> List.tryFindIndex (fun (n, _) -> n = callFunName)
                |> Option.map (fun i -> ctx.funArgs.[i] :?> (obj list -> obj))

            match lambda with
            | Some lambda ->
                let argValues = callArgs |> List.map (invokeNode ctx)
                lambda argValues
            | None ->
                match tryFindFunction ctx.funcs callFunName callArgs with
                | Some (fargTypes, _, fbody) ->
                    let ctx2 =
                        { ctx with
                            argTypes = fargTypes
                            funArgs = callArgs |> List.map (fun argBody -> invokeNode ctx argBody) }

                    fbody |> List.map (fun b -> invokeNode ctx2 b) |> List.last
                | None ->
                    if callFunName.StartsWith(':') then
                        let m: Map<string, obj> = invokeNode ctx callArgs.[0] |> unbox

                        Map.tryFind (callFunName.Substring(1)) m
                        |> Option.defaultWith (fun _ -> failwithf "Cant find value for key %s in %O" callFunName m)
                    else
                        let extFun =
                            ctx.extFuncResolve callFunName (Seq.length callArgs)
                            |> Option.defaultWith (fun _ -> failwithf "Cant find function '%s'" callFunName)

                        let funArgs = callArgs |> List.map (fun argBody () -> invokeNode ctx argBody)

                        extFun funArgs
    | Const x -> box (RSexp x)
    | Fn (argsDesc, _, body) ->
        box (fun (args: obj list) ->
            let localCtx =
                { ctx with
                    localVariables = ctx.argTypes |> List.mapi (fun i (n, _) -> n, ctx.funArgs.[i]) |> Map.ofList
                    funArgs = args
                    argTypes = argsDesc }

            body |> List.map (invokeNode localCtx) |> List.last)
    | NVector xs -> xs |> List.map (invokeNode ctx) |> box
    | NMap xs -> xs |> Map.map (fun _ v -> invokeNode ctx v) |> box
    | n -> failwithf "not implemented for node %O" n

let run extFuncResolve (funcName: string) (funArgs: obj list) (program: Node) : obj =
    match program with
    | Module (_, rootNodes) ->
        let funcs =
            rootNodes
            |> List.filter (function
                | Defn _ -> true
                | _ -> false)

        let (argTypes, _, body) = findFunction funcs funcName funArgs

        let ctx =
            { funcs = funcs
              extFuncResolve = extFuncResolve
              funArgs = funArgs
              argTypes = argTypes
              globalValues = Map.empty
              localVariables = Map.empty }

        let globalValues =
            rootNodes
            |> List.choose (function
                | Def (defName, defBody) -> (defName, invokeNode ctx defBody) |> Some
                | _ -> None)
            |> Map.ofList

        let globalFunValues =
            ctx.funcs
            |> List.map (fun f ->
                match f with
                | Defn (fname, _, _, _) -> fname, box f
                | _ -> failwith "???")
            |> Map.ofList

        let ctx =
            { ctx with globalValues = Map.fold (fun m k v -> Map.add k v m) globalValues globalFunValues }

        body |> List.map (fun b -> invokeNode ctx b) |> List.last
    | n -> failwithf "Unsupported program root node (%O)" n
