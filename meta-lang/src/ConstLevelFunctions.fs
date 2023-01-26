module ConstLevelFunctions

let invoke name (args: string list) =
    match name with
    | "keyword_of_sexp" when List.length args = 1 -> args.[0] |> box
    | "unknown_of_sexp" when List.length args = 1 -> args.[0] |> box
    | "int_of_sexp" when List.length args = 1 -> int args.[0] |> box
    | "float_of_sexp" when List.length args = 1 -> float args.[0] |> box
    | "string_of_sexp" when List.length args = 1 -> args.[0] |> box
    | _ -> failwithf "Function not found (%s) with args %A" name args
