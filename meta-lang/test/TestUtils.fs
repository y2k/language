module TestUtils

open MetaLang

let resolveTypes (n: Node) =
    let env = ExternalTypeResolver.loadDefault ()

    let ctx =
        TypeResolver.defaultContext
        |> TypeResolver.registerFunc "+" ([ Specific "int"; Specific "int" ], Specific "int")
        |> TypeResolver.registerFunc "+." ([ Specific "float"; Specific "float" ], Specific "float")
        |> TypeResolver.registerFunc "int_of_sexp" ([ RawSexp ], Specific "int")
        |> TypeResolver.registerFunc "float_of_sexp" ([ RawSexp ], Specific "float")
        |> TypeResolver.registerFunc "if" ([ Specific "bool"; Unknown; Unknown ], Unknown)

    TypeResolver.resolve' env ctx n
    |> ConstantValidator.validate
        (TypeResolver.fundFunctionByArgs ctx)
        (TypeResolver.findFuncArgType ctx)
        (ConstLevelFunctions.invoke)
