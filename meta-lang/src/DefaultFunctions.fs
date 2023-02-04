module DefaultFunctions

open MetaLang

let asString (instance: obj) =
    match instance with
    | null -> null
    | :? string as s -> s
    | :? RSexp as (RSexp s) when s.StartsWith '"' && s.EndsWith '"' -> s.Substring(1, s.Length - 2)
    | :? RSexp as (RSexp s) -> s
    | s -> failwithf "Can't parse to string: %A (%O)" s (instance.GetType())

let private unwrapRSexp (instance: obj) =
    match instance with
    | :? RSexp as (RSexp s) when s.StartsWith '"' && s.EndsWith '"' -> s.Substring(1, s.Length - 2) |> box
    | :? RSexp as (RSexp s) -> s |> box
    | instance -> instance

let defaultContext =
    TypeResolver.defaultContext
    |> TypeResolver.registerFunc "assoc" ([ Dictionary Map.empty; Keyword; Unknown ], Dictionary Map.empty)
    |> TypeResolver.registerFunc "cons" ([ Unknown; Specific "list" ], Specific "list")
    |> TypeResolver.registerVarArgsFunc "or" Unknown Unknown
    |> TypeResolver.registerFunc ".split" ([ Specific "string"; Specific "string" ], Unknown)
    |> TypeResolver.registerFunc "vec" ([ Unknown ], Specific "list")
    |> TypeResolver.registerFunc "concat" ([ Specific "list"; Specific "list" ], Specific "list")
    |> TypeResolver.registerFunc "get" ([ Unknown; Keyword ], Unknown)
    |> TypeResolver.registerFunc "first" ([ Specific "list" ], Unknown)
    |> TypeResolver.registerFunc "keyword_of_sexp" ([ RawSexp ], Keyword)
    |> TypeResolver.registerFunc "some?" ([ Unknown ], Specific "bool")
    |> TypeResolver.registerFunc "second" ([ Specific "list" ], Unknown)
    |> TypeResolver.registerFunc "rest" ([ Specific "list" ], Specific "list")
    |> TypeResolver.registerFunc "name" ([ Keyword ], Specific "string")
    |> TypeResolver.registerFunc "and" ([ Specific "bool"; Specific "bool" ], Specific "bool")
    |> TypeResolver.registerFunc "not=" ([ Specific "bool"; Specific "bool" ], Specific "bool")
    |> TypeResolver.registerFunc
        "reduce"
        ([ Function([ Unknown ], Specific "bool"); Unknown; Specific "list" ], Specific "list")
    |> TypeResolver.registerFunc "filter" ([ Function([ Unknown ], Specific "bool"); Specific "list" ], Specific "list")
    |> TypeResolver.registerVarArgsFunc "str" Unknown (Specific "string")

let findNativeFunction (name: string) _ =
    match name with
    | "assoc" ->
        Some(fun (args: (unit -> obj) list) ->
            let dic: Map<string, obj> = args[0]() |> unbox
            let key: string = args[1]() |> asString
            let value = args[2]()
            Map.add key value dic |> box)
    | "cons" ->
        Some(fun (args: (unit -> obj) list) ->
            let value = args[0]()
            let list: obj list = args[1]() |> unbox
            value :: list |> box)
    | ".split" ->
        Some(fun (args: (unit -> obj) list) ->
            let instance = args[0]() |> asString
            let separator = args[1]() |> asString
            instance.Split separator |> Seq.map box |> Seq.toList |> box)
    | "if" ->
        Some(fun (args: (unit -> obj) list) ->
            let condition =
                match args.[0] () with
                | :? bool as b -> b
                | :? RSexp as x -> let (RSexp x) = x in System.Boolean.Parse(x)
                | x -> failwithf "Can't parse '%O' to bool" x

            if condition then args.[1] () else args.[2] ())
    | "some?" ->
        Some(fun (args: (unit -> obj) list) ->
            let (l: obj) = args[0]() |> unbox
            not (isNull l))
    | "str" -> Some(fun (args: (unit -> obj) list) -> Seq.fold (fun a x -> a + (x () |> asString)) "" args |> box)
    | "name" ->
        Some(fun (args: (unit -> obj) list) ->
            let (l: obj) = args[0]()
            l |> asString |> box)
    | "first" ->
        Some(fun (args: (unit -> obj) list) ->
            let (l: obj list) = args[0]() |> unbox
            List.head l |> box)
    // | "keyword_of_sexp" ->
    //     Some(fun (args: (unit -> obj) list) ->
    //         let x = args[0]()
    //         x)
    | "=" ->
        Some(fun (args: (unit -> obj) list) ->
            let l = args[0]() |> unwrapRSexp
            let r = args[1]() |> unwrapRSexp
            r = l |> box)
    | "get" ->
        Some(fun (args: (unit -> obj) list) ->
            if isNull (args[0]()) then
                null
            else
                let (m: Map<string, obj>) = args[0]() |> unbox
                let (k: string) = args[1]() |> asString
                Map.tryFind k m |> Option.defaultValue null |> box)
    | "and" ->
        Some(fun (args: (unit -> obj) list) ->
            let (l: bool) = args[0]() |> unbox
            if l then args[1]() |> unbox else false |> box)
    | "or" ->
        Some(fun (args: (unit -> obj) list) ->
            args
            |> List.tryPick (fun argf ->
                match argf () with
                | :? bool as x -> if x then Some(box x) else None
                | :? RSexp as (RSexp x) when x = "false" || x = "true" ->
                    if x = "true" then Some(box (RSexp "true")) else None
                | x when not (isNull x) -> Some x
                | _ -> None)
            |> Option.defaultValue null)
    | "not=" ->
        Some(fun (args: (unit -> obj) list) ->
            let (l: obj) = args[0]() |> unwrapRSexp
            let (r: obj) = args[1]() |> unwrapRSexp
            // printfn "LOG: not= | %O | %O || %O | %O" l (l.GetType()) r (r.GetType())
            l <> r |> box)
    | "rest" ->
        Some(fun (args: (unit -> obj) list) ->
            let (l: obj list) = args[0]() |> unbox
            List.tail l |> box)
    | "second" ->
        Some(fun (args: (unit -> obj) list) ->
            let (l: obj list) = args[0]() |> unbox
            l.[1] |> box)
    | "concat" ->
        Some(fun (args: (unit -> obj) list) ->
            let (l: obj list) = args[0]() |> unbox
            let (r: obj list) = args[1]() |> unbox
            List.concat [ l; r ] |> box)
    | "map" ->
        Some(fun (args: (unit -> obj) list) ->
            let (f: obj list -> obj) = args[0]() |> unbox
            let (xs: obj list) = args[1]() |> unbox
            List.map (fun x -> f [ x ]) xs |> box)
    | "reduce" ->
        Some(fun (args: (unit -> obj) list) ->
            let (f: obj list -> obj) = args[0]() |> unbox
            let (i: obj) = args[1]()
            let (xs: obj list) = args[2]() |> unbox
            List.fold (fun a x -> f [ a; x ] |> box) i xs)
    | "vec" ->
        Some(fun (args: (unit -> obj) list) ->
            let (m: Map<string, obj>) = args[0]() |> unbox
            Map.toSeq m |> Seq.map (fun (k, v) -> box [ box k; box v ]) |> Seq.toList |> box)
    | "filter" ->
        Some(fun (args: (unit -> obj) list) ->
            let (f: obj list -> obj) = args[0]() |> unbox
            let (xs: obj list) = args[1]() |> unbox
            List.filter (fun x -> f [ x ] |> unbox) xs |> box)
    | _ -> None
