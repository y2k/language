module LanguageParser

type private sexp =
    | Atom of string
    | List of sexp list

module private SexpParser =
    open FParsec

    let private patom =
        (choice [ (between (pchar '"') (pchar '"') (manySatisfy (fun ch -> ch <> '"')))
                  many1Satisfy (fun ch -> isLetter ch || isDigit ch || ch = '-') ]
         |>> (fun name -> Atom name))

    let psexp: Parser<_, unit> =
        let expr, exprImpl = createParserForwardedToRef ()

        exprImpl
        := between
            (spaces .>> choice [ pstring "("; pstring "[" ])
            (choice [ pstring ")"; pstring "]" ])
            ((many (choice [ patom; expr ] .>> spaces))
             |>> List)

        expr

    let parse str =
        match run psexp str with
        | Success (x, _, _) -> x
        | Failure (a, b, _) -> failwithf "could not parse (%O %O)" a b

open MetaLang

let rec private compileFuncBody sexp =
    match sexp with
    | List (Atom fname :: args) -> ExtCall(fname, args |> List.map compileFuncBody)
    | Atom sym -> ExtSymbol sym
    | _ -> failwithf "invalid node %O" sexp

let private compileDefn sexp =
    match sexp with
    | List (Atom "defn" :: Atom funcName :: List argsSexp :: body) ->
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
