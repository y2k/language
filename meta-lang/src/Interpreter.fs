module Interpreter

open MetaLang

let private tryFindFunction funcs funcName funArgs =
    funcs
    |> List.tryPick
        (function
        | Defn (n, argTypes, retType, body) when
            n = funcName
            && (List.length argTypes = List.length funArgs)
            ->
            Some(argTypes, retType, body)
        | _ -> None)

let private findFunction funcs funcName funArgs =
    tryFindFunction funcs funcName funArgs
    |> Option.defaultWith (fun _ -> failwithf "Can't find function '%s'" funcName)

type Context =
    private
        { funcs: Node list
          extFuncs: Map<string, obj list -> obj>
          funArgs: obj list
          argTypes: (string * Type) list
          globalValues: Map<string, obj> }

let findValueInContext (ctx: Context) symName : obj =
    ctx.argTypes
    |> List.tryFindIndex (fun (n, _) -> n = symName)
    |> Option.map (fun argIndex -> ctx.funArgs.[argIndex])
    |> Option.orElseWith (fun _ -> Map.tryFind symName ctx.globalValues)
    |> Option.defaultWith (fun _ -> failwithf "Can't find value '%s'" symName)

let rec private invokeNode (ctx: Context) (body: Node) : obj =
    match body with
    | Symbol symName -> findValueInContext ctx symName
    | Call (callFunName, callArgs) ->

        let funInArgs =
            ctx.argTypes
            |> List.tryFindIndex (fun (n, _) -> n = callFunName)
            |> Option.map (fun i -> ctx.funArgs.[i] :?> Node)
            |> Option.map
                (function
                | Defn (_, argTypes, retType, body) -> argTypes, retType, body
                | x -> failwithf "Argument %s must be functions, but it %O" callFunName x)

        match funInArgs with
        | Some (fargTypes, _, fbody) ->
            let ctx2 =
                { ctx with
                      argTypes = fargTypes
                      funArgs =
                          callArgs
                          |> List.map (fun argBody -> invokeNode ctx argBody) }

            fbody
            |> List.map (fun b -> invokeNode ctx2 b)
            |> List.last
        | None ->
            match tryFindFunction ctx.funcs callFunName callArgs with
            | Some (fargTypes, _, fbody) ->
                let ctx2 =
                    { ctx with
                          argTypes = fargTypes
                          funArgs =
                              callArgs
                              |> List.map (fun argBody -> invokeNode ctx argBody) }

                fbody
                |> List.map (fun b -> invokeNode ctx2 b)
                |> List.last
            | None ->
                let extFun =
                    Map.tryFind callFunName ctx.extFuncs
                    |> Option.defaultWith (fun _ -> failwithf "Cant find foreign function '%s'" callFunName)

                let funArgs =
                    callArgs
                    |> List.map (fun argBody -> invokeNode ctx argBody)

                extFun funArgs
    | Const x -> box (RSexp x)
    | n -> failwithf "not implemented for node %O" n

let run extFuncs (funcName: string) (funArgs: obj list) (program: Node) : obj =
    match program with
    | Module (_, rootNodes) ->
        let funcs =
            rootNodes
            |> List.filter
                (function
                | Defn _ -> true
                | _ -> false)

        let (argTypes, _, body) = findFunction funcs funcName funArgs

        let ctx =
            { funcs = funcs
              extFuncs = extFuncs
              funArgs = funArgs
              argTypes = argTypes
              globalValues = Map.empty }

        let globalValues =
            rootNodes
            |> List.choose
                (function
                | Def (defName, defBody) -> (defName, invokeNode ctx defBody) |> Some
                | _ -> None)
            |> Map.ofList

        let globalFunValues =
            ctx.funcs
            |> List.map
                (fun f ->
                    match f with
                    | Defn (fname, _, _, _) -> fname, box f
                    | _ -> failwith "???")
            |> Map.ofList

        let ctx =
            { ctx with
                  globalValues = Map.fold (fun m k v -> Map.add k v m) globalValues globalFunValues }

        body
        |> List.map (fun b -> invokeNode ctx b)
        |> List.last

    | n -> failwithf "Unsupported program root node (%O)" n
