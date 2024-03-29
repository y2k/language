module ExternalTypeResolver

type t =
    private { methods: Map<string, string> }

let private makeKey cls method paramCount paramIndex =
    sprintf "%s-%s-%i-%i" cls method paramCount paramIndex

let loadDefault () : t =
    { methods =
        Map.ofList
            [ (makeKey "android.widget.Toast" "makeText" 3 0), "android.content.Context"
              (makeKey "android.widget.Toast" "makeText" 3 1), "String"
              (makeKey "android.widget.Toast" "makeText" 3 2), "int"
              (makeKey "android.widget.TextView" "<init>" 1 0), "android.content.Context"
              (makeKey "android.widget.TextView" "setText" 1 0), "String"
              (makeKey "java.lang.String" "valueOf" 1 0), "int" ] }

let add cls method type' (t: t) =
    { t with methods = t.methods |> Map.add (makeKey cls method 1 0) type' }

let resolveStatic (t: t) (cls: string) (method: string) (pc: int) (pi: int) : string =
    let key = makeKey cls method pc pi

    Map.tryFind key t.methods
    |> Option.defaultWith (fun _ -> failwithf "Can't resolve type '%s'" key)

let resolveStatic' (t: t) (path: string) (pc: int) (pi: int) =
    let args = path.Split '/'
    resolveStatic t args.[0] args.[1] pc pi

let resolve (t: t) (cls: string) (method: string) (pc: int) (pi: int) : string =
    let key = makeKey cls method pc pi

    Map.tryFind key t.methods
    |> Option.defaultWith (fun _ -> failwithf "Can't resolve type '%s'" key)

let resolve' (t: t) (path: string) (pc: int) (pi: int) : string =
    let args = path.Split '/'
    resolve t args.[0] args.[1] pc pi

let resolveConstructorType (t: t) (cls: string) (pc: int) (pi: int) : string = resolve t cls "<init>" pc pi
