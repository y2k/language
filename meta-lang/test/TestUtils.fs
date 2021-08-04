module TestUtils

open MetaLang

let resolveTypes (n: Node) =
    let env = ExternalTypeResolver.loadDefault ()

    let ctx =
        TypeResolver.defaultContext
        |> TypeResolver.registerFunc "+" [ Specific "int"; Specific "int" ]
        |> TypeResolver.registerFunc "+." [ Specific "float"; Specific "float" ]

    TypeResolver.resolve' env ctx n
