module LanguageParser

type sexp =
    | Atom of string
    | List of sexp list
    | Vector of sexp list
    | SMap of sexp list

module SexpParser =
    open FParsec

    let private patom =
        (choice [ (between (pchar '"') (pchar '"') (manySatisfy (fun ch -> ch <> '"'))
                   |>> (sprintf "\"%s\""))
                  many1Satisfy
                      (fun ch ->
                          isLetter ch
                          || isDigit ch
                          || ch = '-'
                          || ch = '+'
                          || ch = ':') ]
         |>> (fun name -> Atom name))

    let private psexp: Parser<_, unit> =
        let expr, exprImpl = createParserForwardedToRef ()

        exprImpl
        := choice [ between
                        (spaces .>> choice [ pstring "(" ])
                        (choice [ pstring ")" ])
                        ((many (choice [ patom; expr ] .>> spaces))
                         |>> List)
                    between
                        (spaces .>> choice [ pstring "[" ])
                        (choice [ pstring "]" ])
                        ((many (choice [ patom; expr ] .>> spaces))
                         |>> Vector)
                    between
                        (spaces .>> choice [ pstring "{" ])
                        (choice [ pstring "}" ])
                        ((many (choice [ patom; expr ] .>> spaces))
                         |>> SMap) ]

        expr

    let parse str =
        match run psexp str with
        | Success (x, _, _) -> x
        | Failure (a, b, _) -> failwithf "Cant parse (%O %O)" a b

open MetaLang
open System.Text.RegularExpressions

let rec private compileFuncBody sexp =
    match sexp with
    | List (Atom "fn" :: Vector argsSexp :: body) ->
        let args =
            argsSexp
            |> List.map
                (function
                | Atom argName -> argName, Unknown
                | n -> failwithf "invalid node %O" n)

        ExtFn(args, Unknown, body |> List.map compileFuncBody)
    | List (Atom fname :: args) -> ExtCall(fname, args |> List.map compileFuncBody)
    | Atom sym ->
        if sym.StartsWith(':') then
            ExtConst(sym.Substring(1))
        else if sym = "true" || sym = "false" then
            ExtConst sym
        else if Regex.IsMatch(sym, "^[a-z].*$") then
            ExtSymbol sym
        else
            ExtConst sym
    | Vector xs -> ExtVector(xs |> List.map compileFuncBody)
    | SMap xs ->
        xs
        |> List.chunkBySize 2
        |> List.map
            (function
            | Atom k :: v :: [] when k.StartsWith(':') -> k.Substring(1), compileFuncBody v
            | s -> failwithf "Invalid map node %O" s)
        |> Map.ofList
        |> ExtMap
    | _ -> failwithf "invalid node %O" sexp

let private compileDefn sexp =
    match sexp with
    | List (Atom "def" :: Atom valName :: body :: []) -> ExtDef(valName, compileFuncBody body)
    | List (Atom "defn" :: Atom funcName :: Vector argsSexp :: body) ->
        let args =
            argsSexp
            |> List.map
                (function
                | Atom argName -> argName, Unknown
                | n -> failwithf "invalid node %O" n)

        ExtDefn(funcName, args, Unknown, body |> List.map compileFuncBody)
    | _ -> failwithf "invalid node %O" sexp

let compile str =
    match SexpParser.parse str with
    | List (Atom "module" :: methods) -> ExtModule(methods |> List.map compileDefn)
    | p -> failwithf "invalid program (%O)" p
