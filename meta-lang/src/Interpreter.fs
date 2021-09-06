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
          argTypes: (string * Type) list }

let findValueInContext (ctx: Context) symName : obj =
    let argIndex =
        ctx.argTypes
        |> List.tryFindIndex (fun (n, _) -> n = symName)
        |> Option.defaultWith (fun _ -> failwithf "Cant find value for %O" symName)

    ctx.funArgs.[argIndex]

let rec private invokeNode (ctx: Context) (body: Node) : obj =
    match body with
    | Symbol symName -> findValueInContext ctx symName
    | Call (callFunName, callArgs) ->
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
            let funArgs =
                callArgs
                |> List.map (fun argBody -> invokeNode ctx argBody)

            let extFun = ctx.extFuncs.[callFunName]
            extFun funArgs
    | Const x -> box x
    | n -> failwithf "not implemented for node %O" n

let run extFuncs (funcName: string) (funArgs: obj list) (program: Node) : obj =
    match program with
    | Module (_, funcs) ->
        let (argTypes, _, body) = findFunction funcs funcName funArgs

        let ctx =
            { funcs = funcs
              extFuncs = extFuncs
              funArgs = funArgs
              argTypes = argTypes }

        body
        |> List.map (fun b -> invokeNode ctx b)
        |> List.last

    | n -> failwithf "Unsupported program root node (%O)" n
