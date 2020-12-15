module ExternalTypeResolver

type t =
    private
        { methods: Map<string, string> }

let private makeKey cls method pc pi = sprintf "%s-%s-%i-%i" cls method pc pi

let loadDefault (): t =
    { methods =
          Map.ofList [ (makeKey "android.widget.Toast" "makeText" 3 0), "android.content.Context"
                       (makeKey "android.widget.Toast" "makeText" 3 1), "String"
                       (makeKey "android.widget.Toast" "makeText" 3 2), "int"
                       (makeKey "android.widget.TextView" "<init>" 1 0), "android.content.Context"
                       (makeKey "android.widget.TextView" "setText" 1 0), "String" ] }

let resolveStatic (t: t) (cls: string) (method: string) (pc: int) (pi: int): string =
    let key = makeKey cls method pc pi

    Map.tryFind key t.methods
    |> Option.defaultWith (fun _ -> failwithf "Can't resolve type '%s'" key)

let resolveStatic' (t: t) (path: string) (pc: int) (pi: int) =
    let args = path.Split '/'
    resolveStatic t args.[0] args.[1] pc pi

let resolve (t: t) (cls: string) (method: string) (pc: int) (pi: int): string =
    let key = makeKey cls method pc pi

    Map.tryFind key t.methods
    |> Option.defaultWith (fun _ -> failwithf "Can't resolve type '%s'" key)

let resolveConstructorType (t: t) (cls: string) (pc: int) (pi: int): string = resolve t cls "<init>" pc pi
