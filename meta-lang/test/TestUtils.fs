module TestUtils

open MetaLang

let resolveTypes (n: Node) =
    let env = ExternalTypeResolver.loadDefault ()

    let unitType = Specific "unit"

    let ctx =
        TypeResolver.defaultContext
        |> TypeResolver.registerFunc "+" ([ Specific "int"; Specific "int" ], Specific "int")
        |> TypeResolver.registerFunc "+." ([ Specific "float"; Specific "float" ], Specific "float")
        |> TypeResolver.registerFunc "int_of_sexp" ([ RawSexp ], Specific "int")
        |> TypeResolver.registerFunc "float_of_sexp" ([ RawSexp ], Specific "float")
        |> TypeResolver.registerFunc "string_of_sexp" ([ RawSexp ], Specific "string")
        |> TypeResolver.registerFunc "if" ([ Specific "bool"; Unknown; Unknown ], Unknown)
        |> TypeResolver.registerFunc "dic-get" ([ Specific "dic"; Specific "string" ], Unknown)
        |> TypeResolver.registerFunc "ui-render" ([ Specific "dic" ], unitType)
        |> TypeResolver.registerFunc
            "dic-add"
            ([ Specific "dic"
               Specific "string"
               Unknown ],
             Specific "dic")
        |> TypeResolver.registerFunc "update-model" ([ Function([ Specific "dic" ], Specific "dic") ], unitType)

    TypeResolver.resolve env ctx n
    |> ConstantValidator.validate
        (TypeResolver.fundFunctionByArgs ctx)
        (TypeResolver.findFuncArgType ctx)
        (ConstLevelFunctions.invoke)
