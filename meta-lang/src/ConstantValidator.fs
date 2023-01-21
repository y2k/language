module ConstantValidator

open MetaLang

let rec callRecursive f node =
    f node

    match node with
    | Call (_, args) -> args |> List.iter (callRecursive f)
    | Module (_, funcs) -> funcs |> List.iter (callRecursive f)
    | Defn (_, _, _, body) -> body |> List.iter (callRecursive f)
    | Const _
    | Symbol _ -> ()
    | Fn (_, _, body) -> body |> List.iter (callRecursive f)
    | Def (_, body) -> callRecursive f body
    | NMap xs -> Map.values xs |> Seq.iter (callRecursive f)
    | NVector body -> body |> List.iter (callRecursive f)
    | Let (_, body) -> body |> List.iter (callRecursive f)

let private callParseFunForValidConst findFuncArgType findFunctionByArgs invokeFunc name i arg =
    match arg with
    | Const sexp ->
        let (expSign: Type) = findFuncArgType name i

        let (conFuncName, actSign: Type) =
            findFunctionByArgs [ RawSexp ] expSign
            |> Option.defaultWith (fun _ -> failwithf "Can't find function for parse sexp->%O (sexp: %O)" expSign sexp)

        invokeFunc conFuncName [ sexp ] |> ignore
    | _ -> ()

let validate findFunctionByArgs findFuncArgType invokeFunc node =
    callRecursive
        (fun node ->
            match node with
            | Call (name, args) ->
                List.iteri (callParseFunForValidConst findFuncArgType findFunctionByArgs invokeFunc name) args
            | Const _
            | Symbol _
            | Def _
            | Defn _
            | NMap _
            | NVector _
            | Let _
            | Fn _
            | Module _ -> ())
        node

    node
