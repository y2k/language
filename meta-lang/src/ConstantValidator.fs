module ConstantValidator

open MetaLang

let rec callRecursive f node =
    f node

    match node with
    | Call (_, args) -> args |> List.iter (callRecursive f)
    | Module (_, funcs) -> funcs |> List.iter (callRecursive f)
    | Defn (_, _, body) -> body |> List.iter (callRecursive f)
    | Const _
    | Symbol _
    | IsNull _ -> ()
    | Cond _
    | Def _
    | Bind _ -> failwith "not implemented"

let validate fundFunctionByArgs findFuncArgType invokeFunc node =
    callRecursive
        (fun node ->
            match node with
            | Call (name, args) ->
                args
                |> List.iteri
                    (fun i arg ->
                        match arg with
                        | Const sexp ->
                            let (expSign: Type) = findFuncArgType name i
                            let (conFuncName, actSign: Type) = fundFunctionByArgs [ RawSexp ] expSign
                            invokeFunc conFuncName [ sexp ] |> ignore
                        | _ -> ())
            | Const _
            | Symbol _
            | Cond _
            | Bind _
            | Def _
            | Defn _
            | Module _
            | IsNull _ -> ())
        node

    node
