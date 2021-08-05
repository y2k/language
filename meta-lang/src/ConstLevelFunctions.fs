module ConstLevelFunctions

let invoke name (args: string list) =
    match name with
    | "int_of_sexp" when List.length args = 1 -> int args.[0] |> box
    | "float_of_sexp" when List.length args = 1 -> float args.[0] |> box
    | _ -> failwithf "Function not found (%s %A)" name args
